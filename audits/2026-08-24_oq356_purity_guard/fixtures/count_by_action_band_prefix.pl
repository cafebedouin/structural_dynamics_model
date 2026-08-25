%% count_by_action_band(+Members, +Ctx, +SoundFloor, +DegFloor, -NS, -NB, -NW, -ND)
%  Recomputes effective purity for members and counts by zone.
count_by_action_band(Members, Ctx, SoundFloor, DegFloor, NS, NB, NW, ND) :-
    config:param(purity_action_escalation_floor, EscFloor),
    findall(EP,
        (   member(C, Members),
            catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, fail),
            EP >= 0.0
        ),
        EPs),
    count_in_zone(EPs, SoundFloor, 1.01, NS),
    count_in_zone(EPs, EscFloor, SoundFloor, NB),
    count_in_zone(EPs, DegFloor, EscFloor, NW),
    count_in_zone(EPs, -0.01, DegFloor, ND).
