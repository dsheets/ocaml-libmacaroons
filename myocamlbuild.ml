open Ocamlbuild_plugin;;

dispatch begin
  function
  | After_rules ->
    (* flag ["compile"; "ocaml"] (A"-safe-string"); *)

    flag ["link"; "library"; "ocaml"; "byte"; "use_macaroons"]
      (S[A"-dllib"; A("-llibmacaroons"); A"-cclib"; A("-lmacaroons")]);

    flag ["link"; "library"; "ocaml"; "native"; "use_macaroons"]
      (S[A"-cclib"; A("-lmacaroons")]);

    flag ["link"; "program"; "ocaml"; "byte"; "use_macaroons"]
      (S[A"-dllib"; A("-llibmacaroons")]);

    flag ["link"; "program"; "ocaml"; "native"; "use_macaroons"]
      (S[A"-cclib"; A("-lmacaroons")]);

  | _ -> ()
end;;
