/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury.tests

import guillotine._, environments.enclosing
import fury.error._

sealed trait Op[-E <: Exception]

case class Container(id: String) {
  case class Dir(dir: String) {

    def run(cmd: Command): String = {
      val shellCmd = sh"docker exec --workdir $dir $id sh -c ${cmd}"
      shellCmd
        .exec[Outcome[String]]
        .recover(
            on[ShellFailure].map { case ShellFailure(cmd, out, error) => s"$out\n$error" }
        )
    }
  }

  lazy val alpha   = Dir("/work/alpha")
  lazy val beta    = Dir("/work/beta")
  lazy val gamma   = Dir("/work/gamma")
  lazy val delta   = Dir("/work/delta")
  lazy val epsilon = Dir("/work/epsilon")
  lazy val zeta    = Dir("/work/zeta")
  lazy val eta     = Dir("/work/eta")
  lazy val theta   = Dir("/work/theta")
  lazy val iota    = Dir("/work/iota")
  lazy val kappa   = Dir("/work/kappa")

  def stop(): Outcome[Unit] =
    for {
      killed  <- sh"docker kill $id".exec[Outcome[String]]
      removed <- sh"docker rm $id".exec[Outcome[String]]
    } yield ()
}

object Docker {

  def start(): Outcome[Container] =
    sh"docker run --detach fury:latest".exec[Outcome[String]].map(Container(_))

  def prune(): Outcome[String] =
    sh"docker system prune --force".exec[Outcome[String]]

}
