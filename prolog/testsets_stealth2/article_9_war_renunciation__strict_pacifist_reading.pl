% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation — Strict Pacifist Reading (Categorical Prohibition on All Armed Forces)
 *   domain: constitutional law/security policy/institutional legitimacy
 *
 * SUMMARY:
 *   Japan's Article 9 renounces war and provides that 'land, sea, and air
 *   forces, as well as other war potential, will never be maintained.' This
 *   story instantiates ONE reading of that contested kernel — the strict
 *   pacifist reading — under which the text's 'never be maintained' is a
 *   categorical prohibition covering all organized military capacity,
 *   defensive included, leaving national defense to non-military means and to
 *   dependence on the US alliance. The ε referent is fixed by the
 *   kernel-reading rule: the standing arrangement under contest — Japan's
 *   actual security arrangement: the Self-Defense Forces, the US alliance
 *   structure, the reinterpretation machinery, the ongoing buildup — assessed
 *   by this reading's own lights, for which that arrangement is a standing
 *   and worsening violation of the covenant, extractive of Okinawan
 *   territory, of civilian fiscal priorities, and of the constitutional
 *   identity itself. The sibling readings (inherent_right_reading,
 *   collective_self_defense_reading) are separate constraints with their own
 *   ε values and victim sets; they are linked through the network, not folded
 *   into this one. The claim/metric gap is deliberate: claimed_type is
 *   tangled_rope on structural grounds — the constraint coordinates a genuine
 *   collective-action problem while extracting asymmetrically through the
 *   same structure — and the metrics describe its observed operation,
 *   including the erosion this reading exists to indict.
 *
 * KEY AGENTS:
 *   - pacifist_civil_society: primary beneficiary (organized/identity_locked) — collects the covenant's normative force; its identity is fused to the constraint
 *   - japanese_state: primary target (institutional/constrained) — bears the security-autonomy sacrifice; holds the unused amendment lever
 *   - united_states_alliance_partner: dual-positioned beneficiary-payer (institutional/mobile) — receives the dependence the prohibition creates, bears the defense burden it outsources
 *   - okinawan_base_host_communities: concentrated payer and excluded voice (moderate/trapped) — bears the territorial price of alliance dependence
 *   - cabinet_of_japan: agenda-setter via the interpretation lever (institutional/constrained) — administers the constraint's erosion
 *   - national_diet: agenda-setter via the amendment lever (institutional/constrained) — holds the exit in reserve, never exercises it
 *   - supreme_court_of_japan: analytical abstainer (institutional/trapped) — the seat that could adjudicate the kernel and declines
 *   - east_asian_neighbor_states: free-riding beneficiaries (institutional/mobile)
 *   - jsdf_service_members: payers serving under denied legitimacy (institutional/constrained)
 *   - japanese_public: near-symmetric beneficiary-payer (moderate/constrained) — peace dividend received, dependence risk paid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.75).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.28).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation — Strict Pacifist Reading (Categorical Prohibition on All Armed Forces)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional law/security policy/institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '6a1c243a-bb61-476e-9d0d-0106635d3ea6').
narrative_ontology:cs_kernel_codification('6a1c243a-bb61-476e-9d0d-0106635d3ea6', fixed_text).
narrative_ontology:cs_authority_grounding('6a1c243a-bb61-476e-9d0d-0106635d3ea6', distributed).
narrative_ontology:cs_reading_relation('6a1c243a-bb61-476e-9d0d-0106635d3ea6', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6a1c243a-bb61-476e-9d0d-0106635d3ea6', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('6a1c243a-bb61-476e-9d0d-0106635d3ea6', foundational, armed_forces_categorically_impermissible).
narrative_ontology:cs_axiom_status(armed_forces_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('6a1c243a-bb61-476e-9d0d-0106635d3ea6', armed_forces_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('6a1c243a-bb61-476e-9d0d-0106635d3ea6', secondary, nonmilitary_security_sufficiency).
narrative_ontology:cs_axiom_status(nonmilitary_security_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6a1c243a-bb61-476e-9d0d-0106635d3ea6', nonmilitary_security_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('6a1c243a-bb61-476e-9d0d-0106635d3ea6', absolute_renunciation_founding_settlement).
narrative_ontology:cs_drift_state('6a1c243a-bb61-476e-9d0d-0106635d3ea6', post_reinterpretation_buildup_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6a1c243a-bb61-476e-9d0d-0106635d3ea6', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, east_asian_neighbor_states).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, japanese_public).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, okinawan_base_host_communities).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, jsdf_service_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_public).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Peace-movement organizations, Article 9 defense associations, strict-constructionist constitutional scholars, and citizens for whom the renunciation clause is a core national identity commitment. They collect the constraint's continued normative force: no conscription, no war participation since 1945, the peace-nation self-concept, and the litigation and electoral mobilization that hold the line against revision. Leaving the arrangement would mean abandoning the identity the constraint anchors; the movement's own aging and generational turnover is the slow erosion of this seat.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society, beneficiary,
    organized, generational, identity_locked, national).

% Receives the peace dividend — no conscription, no citizen war deaths since 1945, defense spending historically held near one percent of GDP — and shares the constitutional identity. Bears the dependence risk: in a crisis their defense depends on the will of an external ally and on forces this reading says the constitution forbids them to have. Pays indirectly through host-nation support appropriations and through the base burden concentrated on Okinawa.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, japanese_public, payer).

% Regional states that cite the renunciation clause in their own security planning and diplomacy. They receive the security-dilemma relief of a constitutionally locked non-militarized neighbor without bearing any of its costs — free riders on the constraint whose own armament choices the clause does not touch. Their reactions (arms racing, diplomatic pressure) set the political price of any Japanese exit from it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, east_asian_neighbor_states, beneficiary,
    institutional, generational, mobile, continental).

% Provides the defense capacity this reading forbids Japan to maintain, in exchange for basing rights, host-nation support payments, and an ally structurally unable to independent-ize its security policy or entrap it in a war of Japan's choosing. Receives the dependence the prohibition creates; simultaneously bears the defense burden the prohibition outsources and spends diplomatic capital pressing Japan to reinterpret the clause and rearm. Its revealed position opposes this reading; its structural position receives its benefits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner, payer).

% The constitutional person whose security autonomy this reading sacrifices: barred from maintaining organized military capacity of any kind, its crisis options reduce to non-military instruments and invocation of an ally's will. Its defense establishment's policy preference — normalized armed forces — is constitutionally blocked on this reading. It retains the formal amendment power (the Article 96 two-thirds-plus-referendum path) that could dissolve the constraint, and in seventy-eight years has never once exercised it for this clause.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state, payer,
    institutional, generational, constrained, national).

% Communities hosting the overwhelming share of the US bases that the alliance dependence this reading necessitates requires — the concentrated territorial price of a defense outsourced to an ally. They bear noise, crime, land takings, and accident exposure the rest of the national territory largely escapes, and they have voted against base expansion in referendums the central government has proceeded past. Their communities cannot relocate the bases and cannot secede from them.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, okinawan_base_host_communities, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, okinawan_base_host_communities, excluded).

% The roughly quarter-million personnel of the Self-Defense Forces, serving in an institution whose legitimacy this reading denies outright. Their deployments draw constitutional litigation (one high court found the Iraq deployment unconstitutional in part), their institution's name and legal status are artifacts of the judicial avoidance the clause forces, and their careers ride on a reading contest they cannot themselves resolve. Exit means leaving the profession; staying means serving under perpetual legality doubt.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, jsdf_service_members, payer,
    institutional, biographical, constrained, national).

% Holds the interpretation lever: Cabinet determinations have moved the clause's official meaning without amendment — most decisively the 2014 determination authorizing collective self-defense, which declared the text unchanged while changing what it permits. Sets the pace of the constraint's erosion within the bounds of public opinion and coalition politics, and claims continuity with the text while practice departs from it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, cabinet_of_japan, agenda_setter,
    institutional, biographical, constrained, national).

% Holds the amendment lever: a two-thirds vote in both houses plus a majority referendum could rewrite or delete the clause. No Diet in seventy-eight years has brought an Article 9 amendment to a vote, because the electoral risk of the referendum has always exceeded the coalition benefit. The constraint's persistence is therefore partly this seat's repeated choice not to act — an agenda held in reserve rather than exercised.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, national_diet, agenda_setter,
    institutional, biographical, constrained, national).

% The seat with formal authority to say which reading the text bears, which has declined for seventy-eight years to say. It has never squarely ruled on the clause's meaning; in the one case reaching the merits it found a partial unconstitutionality and ordered nothing. Its avoidance preserves the ambiguity on which all three readings operate, and it cannot exit the position: ruling for any reading would spend institutional capital it has shown it will not spend.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, supreme_court_of_japan, observer,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of Japanese non-militarization: neighbors and the domestic public cannot verify restraint from policy alone, so the entrenched constitutional text converts restraint into a verifiable, amendment-resistant commitment, and coordinates domestic fiscal and identity priorities around the postwar peace settlement.
% TRANSFER_FUNCTION: Moves security capacity from the Japanese state — prohibited from maintaining any — to the US alliance structure, which provides defense in exchange for basing rights and host-nation support; moves the territorial costs of that defense onto Okinawan communities; moves crisis-time defense risk onto the general population; and moves the fiscal share not spent on forces toward civilian purposes.
% ABSENT_VOICES: Okinawan communities bear the constraint's most concentrated costs but had no seat in the reinterpretation process — the 2014 determination was a Cabinet decision in Tokyo, and Okinawan referendums against base expansion have been proceeded past. JSDF personnel seeking a square judicial answer on their institution's legality have been denied that clarification for decades. The generations who would fight, or be abandoned, in a crisis have no seat in any of the reading contests.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, the amendment project the ruling coalition already holds would complete: forces would be normalized in name and doctrine, the one-percent spending norm would formalize upward, neighbors would answer with their own buildups, and the pacifist identity structure anchoring one of the world's largest peace movements would lose its constitutional anchor. The postwar East Asian security equilibrium is arranged around this clause.
% FOUNDING_PROBLEM: A defeated militarist empire whose armed forces had waged aggressive war across Asia for fifteen years; the clause was built to make the re-emergence of those forces constitutionally impossible — 'land, sea, and air forces, as well as other war potential, will never be maintained' — and to anchor a disarmed Japan in the emergent UN collective-security order.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated outside the beneficiary set by the Tokyo trials record and the occupation-era documents that produced the clause. Its status splits along the reading lines: East Asian neighbor states' official positions and their repeated diplomatic citations of the clause attest that fear of Japanese remilitarization remains live in regional planning; the ruling coalition's platform, revisionist constitutional scholarship, and the US government's burden-sharing demands attest the view that the founding problem is dead and the present danger is under-defense. No attestation outside the contest settles it — the corroboration itself is the contest.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.75 with the referent fixed by the kernel-reading rule: the standing arrangement under contest — the JSDF/alliance/buildup complex — as this reading assesses it. The trajectory (0.12 at the founding settlement to 0.75 today) is the reading's indictment curve: each step of JSDF expansion, overseas deployment, reinterpretation, and buildup raises the arrangement's extractiveness as the strict pacifist sees it. The low 1947 value is not the endorsed alternative as referent — it is the historical fact that the standing arrangement at that moment approximated the settlement this reading endorses; the series measures the arrangement's subsequent drift. Suppression is low and falling (scalar 0.28, matching the end of the suppression_requirement series): the constraint's enforcement machinery — mass civic mobilization, scholarly consensus, judicial willingness, the political spending ceiling — has decayed for decades; this story specifically traces enforcement-capacity change, which is why suppression_requirement is authored. Theater (0.42, rising): as the prohibition's bite narrowed, its maintenance grew more ceremonial — commemorations, identity rituals, reaffirmations — alongside still-real residual function (no conscription, litigation chill, the brake on official doctrine). Accessibility_collapse is low (0.25): the sibling readings remain fully accessible — indeed the inherent-right reading is the government's official position — so understanding this constraint collapses no alternatives. Resistance is high (0.75): the erosion coalition (Cabinet determinations, the Diet's amendment platform, US pressure, the defense establishment) contests the constraint continuously. Coordination type is identity_coordination: the constraint's dominant function is boundary-maintenance of a national identity — the peace-nation self-concept — against evolving criteria, and its failure mode is identity erosion rather than resource misallocation. Receipt surface: the extraction's gains demonstrably accrue to the alliance-partner seat — basing rights, host-nation support, and the dependence-leverage the prohibition's alliance-dependence mechanism creates — so gain_flow names that seat; fixing_cost is prohibitive, because the Article 96 path (two-thirds of both houses plus referendum) has never been survivable for any coalition relative to the benefit of fixing. The claimed type, tangled_rope, is stated from structure and is not reconciled to the metrics.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the pacifist seat the constraint IS the covenant — the thing that makes Japan's peace real — and its erosion is the loss. From the state seat the same clause is an amputation of sovereign capacity that no comparable state tolerates. From the Okinawan seat the constraint's alliance-dependence chain concentrates costs their communities cannot refuse or relocate. From the US seat the prohibition is a bargain: basing, leverage, and a non-entrapping ally, against a defense burden it would rather share. The Cabinet and Diet hold levers, not positions — their experience of the constraint is the electoral price of touching it. The Court experiences it as a trap in which every possible ruling costs more than avoidance. The engine computes per-seat classifications from these structural positions; the divergence between the pacifist seat's subsidized reading and the payer seats' extraction readings is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: pacifist_civil_society and east_asian_neighbor_states are pure recipients — the neighbors pay nothing and exit freely, the purest free ride in the story. japanese_public sits near-symmetric: peace dividend received, dependence risk and indirect costs paid. Victim declarations drive high directionality: japanese_state bears the security-autonomy sacrifice, okinawan_base_host_communities bear concentrated territorial costs with trapped exit (higher still), jsdf_service_members serve under denied legitimacy. The dual-positioned seats derive intermediate d: the US receives basing, leverage, and dependence while bearing the outsourced burden and actively contesting the constraint — its beneficiary and payer roles pull in opposite directions and the derivation lands it mid-low rather than subsidized. No directionality overrides are authored: the role and exit declarations already differentiate the seats, and per-power-atom override granularity could not distinguish the same-atom institutional seats (state, cabinet, diet, court, US, JSDF) whose directionalities genuinely diverge — differentiating them is the role layer's job, and it does it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — irreversible demilitarization of a defeated militarist empire — is contested: the pacifist seat holds it live (the erosion is the proof of need), the revisionist coalition holds it dead (the danger has inverted). The classification machinery prevents both mislabelings. Reading the constraint as pure coordination (rope) would erase the asymmetric extraction — the security autonomy, the Okinawan burden, the outsourced defense — that the payer seats bear. Reading it as pure extraction (snare) would erase the genuine, load-bearing coordination function — the credible-commitment lock that neighbors explicitly cite and that no ordinary policy instrument could replace, held in place by democratic entrenchment rather than coercive suppression of exits. Tangled rope holds both truths. The temporal data shows characteristic tangled-rope drift running in an unusual direction: the extractiveness of the arrangement the reading indicts accumulates while the constraint's own enforcement decays — rising theater, falling suppression, rising extractiveness. That is not yet the piton signature (the function is real, contested, and load-bearing), but if the erosion completes, the residual becomes exactly the piton profile: ceremonial maintenance of a prohibition nobody enforces and nobody obeys. Mandatrophy is not declared resolved — whether the mandate has outlived its function is precisely what the three readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the strict_pacifist_reading of the article_9_war_renunciation kernel. Which reading authoritatively governs — and is the disagreement resolvable at all within the existing adjudication structure, given the Court''s seventy-eight-year avoidance?',
    'A square Supreme Court ruling on the clause''s scope, or an Article 96 amendment that rewrites or deletes the text; absent either, the contest persists by construction.',
    'If the inherent-right or collective-self-defense reading prevails authoritatively, this constraint''s victim set (state security autonomy, JSDF illegitimacy) dissolves into a bounded prohibition on offensive war; if the strict reading prevails, the standing arrangement''s entire force structure becomes unconstitutional and the measured extraction inverts onto the arrangement itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Article 9 kernel governs, and whether the reading contest is adjudicable.').

omega_variable(
    nonmilitary_sufficiency_empirics,
    'Can non-military means plus alliance dependence actually secure Japan against its current threat environment, or does the reading''s instrumental axiom fail empirically?',
    'Crisis performance and deterrence analysis: whether alliance dependence held under coercion short of war (the Senkaku pressure period is the live test), and whether deterrence-by-alliance substitutes for national forces.',
    'If non-military means are insufficient, the constraint''s extraction (security autonomy) purchases a good that fails its purpose and the pacifist coordination claim collapses toward pure cost; if sufficient, the extraction is the working price of a functioning peace mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nonmilitary_sufficiency_empirics, empirical, 'Whether the reading''s instrumental axiom (non-military sufficiency) survives contact with the threat environment.').

omega_variable(
    okinawan_burden_attribution,
    'Is the Okinawan base burden a cost of THIS constraint (the prohibition necessitating alliance dependence) or of the alliance arrangement as a separate constraint this reading would also abolish?',
    'Counterfactual analysis under the sibling readings — whether a Japan armed under the inherent-right reading would materially shrink the US base footprint — plus historical analysis of base-concentration decisions taken under occupation and the security treaty rather than under Article 9''s operation.',
    'If the burden is attributable to the alliance rather than the prohibition, okinawan_base_host_communities exits this story''s victim set and the constraint''s extraction profile narrows to security autonomy alone; if attributable, the victim set stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(okinawan_burden_attribution, conceptual, 'Attribution of the alliance''s territorial costs between the prohibition and the alliance arrangement.').

omega_variable(
    enforcement_decay_or_normalization,
    'Is the falling suppression_requirement trajectory enforcement decay (the constraint losing coercive capacity) or successful normalization (the commitment internalized, requiring less enforcement)?',
    'Counterfactual test — whether remilitarization would proceed immediately if all remaining enforcement (litigation risk, the identity brake, the electoral constraint) ceased — plus generational polling on the clause''s support.',
    'Decay puts the constraint on a piton trajectory (function atrophying, maintenance turning ceremonial); normalization means the constraint is robust at lower cost and the rising theater reading is wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_or_normalization, empirical, 'Whether declining enforcement reflects decay or internalization.').

omega_variable(
    pacifist_identity_renewability,
    'Is the pacifist seat''s identity-locked exit permanent, or is the identity generational — eroding as the war-generation and its immediate heirs exit the population?',
    'Generational cohort analysis of Article 9 support and peace-movement membership; whether the identity renews through institutions (education, commemorations, constitutional litigation) or only through living memory.',
    'If the identity erodes generationally, the constraint''s primary beneficiary seat thins and enforcement decay accelerates beyond the modeled trajectory; if it renews institutionally, the constraint''s persistence is more robust than the suppression series suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pacifist_identity_renewability, empirical, 'Whether the identity lock anchoring the constraint''s main beneficiary seat is renewable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art9_strict_pacifist_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1972, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1972, 0.16).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1985, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1985, 0.19).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1995, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2001, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2014, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2014, 0.33).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2020, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2025, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(art9_strict_pacifist_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.12).
narrative_ontology:measurement(art9_strict_pacifist_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(art9_strict_pacifist_be_t1972, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1972, 0.44).
narrative_ontology:measurement(art9_strict_pacifist_be_t1985, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(art9_strict_pacifist_be_t1995, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(art9_strict_pacifist_be_t2001, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(art9_strict_pacifist_be_t2014, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2014, 0.68).
narrative_ontology:measurement(art9_strict_pacifist_be_t2020, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(art9_strict_pacifist_be_t2025, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(art9_strict_pacifist_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.7).
narrative_ontology:measurement(art9_strict_pacifist_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(art9_strict_pacifist_su_t1972, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1972, 0.66).
narrative_ontology:measurement(art9_strict_pacifist_su_t1985, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(art9_strict_pacifist_su_t1995, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(art9_strict_pacifist_su_t2001, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(art9_strict_pacifist_su_t2014, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2014, 0.36).
narrative_ontology:measurement(art9_strict_pacifist_su_t2020, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(art9_strict_pacifist_su_t2025, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 9' covers three structurally distinct constraints — one per reading of the kernel. They differ in victim set (the strict reading's includes the state's security autonomy and the JSDF's legitimacy; the inherent-right reading's is confined to offensive war capacity; the collective reading's extends to alliance entrapment risk), in ε (each reading assesses the same standing arrangement by its own lights), and in enforcement profile. This story is upstream-most in textual authority (the plain meaning of 'never be maintained') and the most resisted in practice. Each family member links the others via affects_constraints; no story folds the contest into its own classification, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
