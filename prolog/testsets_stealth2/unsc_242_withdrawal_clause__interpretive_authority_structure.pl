% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC 242 Withdrawal Clause — Contested Interpretive Authority Structure
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242 (November 1967) governs withdrawal after
 *   the Six-Day War, but its English text ('from territories occupied') and
 *   French text ('des territoires occupés') diverge on scope, and no
 *   institution holds recognized authority to fix the meaning. This file
 *   instantiates ONE reading of that kernel — the
 *   interpretive_authority_structure reading: the standing arrangement in
 *   which the International Court of Justice claims the question as judicial
 *   interpretation, the drafting states claim it as authorial intent, and the
 *   administering state claims it as customary practice, with none supreme.
 *   Per the ε-invariance rule, the substantive scope contests are OTHER
 *   constraints (maximal_withdrawal_reading, partial_withdrawal_reading),
 *   linked via network.affects_constraints; this story's ε referent is the
 *   standing interpretive-authority arrangement itself, assessed by this
 *   reading's own lights — under which the meta-dispute is the load-bearing
 *   mechanism that perpetuates the substantive ambiguity, transferring
 *   settlement-deferral benefits to parties with veto or non-cooperation
 *   capacity and imposing indefinite deferral on parties seeking legal
 *   closure. KEY AGENTS (by structural relationship):
 *   permanent_five_veto_holders — gatekeepers (institutional/arbitrage), veto
 *   referral or enforcement of any definitive reading; occupying_state —
 *   primary beneficiary (powerful/constrained), receives continued
 *   administrative control while no authoritative scope exists;
 *   drafting_states — secondary beneficiaries (institutional/arbitrage),
 *   authorial-intent claim preserves leverage; icj — claimant to judicial
 *   authority (institutional/constrained), acts when seised but acceptance is
 *   withheld; displaced_and_occupied_populations — primary targets
 *   (powerless/trapped); arab_frontline_states — closure-seeking parties
 *   (organized/constrained); un_general_assembly_majority — institutional
 *   dissenter (organized/constrained); international_law_scholarship —
 *   analytical observer (analytical/analytical).
 *
 * KEY AGENTS:
 *   - permanent_five_veto_holders: gatekeepers (institutional/arbitrage) — veto any binding referral or enforcement of a definitive reading; primary beneficiaries of the standing contest
 *   - occupying_state: primary beneficiary (powerful/constrained) — collects continued administrative control for as long as the clause's scope stays undetermined
 *   - drafting_states: secondary beneficiaries (institutional/arbitrage) — authorial-intent claim preserves interpretive leverage decades after adoption
 *   - icj: claimant to judicial authority (institutional/constrained) — exercises interpretive power when seised; the states best placed to trigger or block its seisin contest its competence
 *   - displaced_and_occupied_populations: primary targets (powerless/trapped) — bear indefinite deferral of status, return, and compensation
 *   - arab_frontline_states: closure-seeking parties (organized/constrained) — pursue a defined withdrawal obligation through war, bilateral treaty, and collective diplomacy
 *   - un_general_assembly_majority: institutional dissenter (organized/constrained) — records preferred readings annually without power to bind
 *   - international_law_scholarship: analytical observer (analytical/analytical) — documents drafting history and maps acceptance and rejection of each authority claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Withdrawal Clause — Contested Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '4c871735-5593-4221-b540-28bb98cf4b4f').
narrative_ontology:cs_kernel_codification('4c871735-5593-4221-b540-28bb98cf4b4f', fixed_text).
narrative_ontology:cs_authority_grounding('4c871735-5593-4221-b540-28bb98cf4b4f', distributed).
narrative_ontology:cs_reading_relation('4c871735-5593-4221-b540-28bb98cf4b4f', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('4c871735-5593-4221-b540-28bb98cf4b4f', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('4c871735-5593-4221-b540-28bb98cf4b4f', foundational, no_binding_interpretation_without_consent).
narrative_ontology:cs_axiom_status(no_binding_interpretation_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('4c871735-5593-4221-b540-28bb98cf4b4f', no_binding_interpretation_without_consent, conventional).
narrative_ontology:cs_axiom('4c871735-5593-4221-b540-28bb98cf4b4f', secondary, unanimity_text_priority_over_determinacy).
narrative_ontology:cs_axiom_status(unanimity_text_priority_over_determinacy, holdable).
narrative_ontology:cs_axiom_grounding('4c871735-5593-4221-b540-28bb98cf4b4f', unanimity_text_priority_over_determinacy, instrumental).
narrative_ontology:cs_reference_frame('4c871735-5593-4221-b540-28bb98cf4b4f', consensual_interpretive_pluralism).
narrative_ontology:cs_drift_state('4c871735-5593-4221-b540-28bb98cf4b4f', contemporary_post_advisory_opinion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c871735-5593-4221-b540-28bb98cf4b4f', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_five_veto_holders).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_and_occupied_populations).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, arab_frontline_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, un_general_assembly_majority).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, consent_based_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, security_council_veto_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over any Council decision that would send the withdrawal question to a binding forum or attach enforcement to an interpretation of the clause. Each has used or threatened the veto to keep the question off the Council's binding agenda, and each invokes or discounts the drafting record according to current alignment. Their consent is the gate any definitive resolution must pass; none has an interest in surrendering the gate, and each can engage or disengage from interpretive commitments case by case.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_five_veto_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_five_veto_holders, beneficiary).

% Administers territories captured in 1967 whose required withdrawal the clause leaves undefined. Asserts that customary state practice and its own security requirements, not any tribunal or the drafting record, determine what the text obliges. Receives continued administrative control for as long as no authoritative reading fixes a scope; bears recurring condemnation, isolation costs, and the burden of open-ended administration. Could submit the question to binding third-party determination and has declined every such path for five decades.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, constrained, regional).

% Authored the English and French texts whose articles diverge, and retain standing to testify to what they meant. Invoke authorial intent when it supports current policy and set it aside when it does not; their interpretive relevance persists only while the ambiguity stays unresolved, and they select among forums and arguments opportunistically.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, beneficiary,
    institutional, generational, arbitrage, global).

% Claims authority as the principal judicial organ to fix the clause's meaning when seised, and has issued advisory opinions touching the occupation's legality. Its determinations bind no state that withholds consent, and the governments with the greatest capacity to trigger or block its seisin are the same ones contesting its competence. Its institutional interest lies in its interpretations being accepted, which the standing contest withholds; it can act only when asked, and its requests route through the same political bodies that maintain the deadlock.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj, agenda_setter,
    institutional, civilizational, constrained, global).

% Live under administration or in exile following the 1967 displacement, on territory whose status the unresolved clause governs. Had no seat in the drafting and hold none in the interpretive contest among governments; each further decade of indeterminacy extends their condition. Leaving would mean abandoning homes, claims, or the possibility of return, so they remain where the arrangement's consequences land heaviest and its conversations happen least.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_and_occupied_populations, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_and_occupied_populations, excluded).

% Entered the postwar settlement demanding a defined withdrawal obligation and have pursued closure through war, bilateral treaties, and collective diplomacy. Egypt and Jordan later settled bilaterally on terms short of a general ruling, leaving the general question open; the remaining parties bear recurrent confrontation and mobilization costs without a determinate legal anchor, and their coalitions fracture whenever a member settles separately.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, arab_frontline_states, payer,
    organized, generational, constrained, regional).

% Passes near-annual resolutions affirming particular readings of the clause and requesting advisory opinions, spending diplomatic capital on instruments the veto-holding minority and the administering state disregard. Its members cannot compel the Council or the Court; participation records dissent and builds the record without producing closure, and abstaining would concede the interpretive field entirely.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, un_general_assembly_majority, payer,
    organized, generational, constrained, global).

% Documents the drafting history, maps the competing interpretive claims, and tracks which authorities states accept or reject over time. Produces the record any eventual resolution will rely on and the running audit of each claimant's consistency, but commands no compliance of its own and bears none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Resolution 242 as the single mutually cited reference framework for Middle East peace diplomacy: every party, however opposed, continues to negotiate 'on the basis of 242,' which preserves a common negotiating vocabulary across otherwise incompatible positions and keeps multilateral process possible at all.
% TRANSFER_FUNCTION: Moves interpretive control and settlement-deferral benefit from parties seeking legal closure (displaced and occupied populations, frontline states, the Assembly majority) to parties holding veto or non-cooperation capacity (the permanent five, the administering state, the drafting states); it moves no goods directly, but it allocates the right to define 'withdrawal' — and therefore the disposition of territory — indefinitely into the future.
% ABSENT_VOICES: The displaced and occupied populations themselves — the people whose territory and status the clause governs — had no seat in the 1967 drafting and hold none in the interpretive contest; they appear in the various readings only as objects. Small and middle powers without veto likewise cannot compel resolution and are present only as voting majorities whose outputs are disregarded. Both groups would insist that a question about their disposition cannot be settled without them, and both are structurally outside the rooms where the authority claims are traded.
% DISAPPEARANCE_RATIONALE: If a universally accepted interpreter appeared overnight, the substantive contest would collapse to a single reading: the administering state's obligations would become determinate, the closure-seeking parties' claims would crystallize into enforceable form, and the negotiation architecture built around managed ambiguity — land-for-peace formulas, interim arrangements, bilateral exceptions — would rearrange around the ruling. Every party's position depends on which way authority resolves, which is precisely why all of them fight over it; nothing about the current arrangement survives its disappearance intact.
% FOUNDING_PROBLEM: In October–November 1967 the Security Council needed language all permanent members could adopt after the Six-Day War: the United States and Britain aligned with the administering state's preference for flexibility, the Soviet Union with the Arab demand for totality, and the English indefinite article against the French definite article was the compromise that bought unanimity. The founding problem was producing an adoptable text, not determining withdrawal scope.
% FOUNDING_PROBLEM_CORROBORATION: Declassified United States, British, and Soviet diplomatic archives and independent treaty-interpretation scholarship on the drafting history corroborate that the article divergence was a known price paid for unanimity — attested from outside the benefiting parties, who do not dispute the genealogy. Notably, the live contest is over who may fix the meaning now, not over how the ambiguity arose; the corroboration is unusually uncontested because the founding story embarrasses no current position.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because the arrangement's output is indefinite deferral: every year the authority question stays open is a year the closure-seeking parties' claims stay unenforceable while the veto-holding and administering parties keep what they hold. Suppression (0.62) is a raw structural property, unscaled by the engine: it consists of veto deployment, refusal of consent to jurisdiction, and non-cooperation with advisory outputs — institutional blocking rather than physical coercion. Theater ratio (0.58) reflects that the majority of observable activity — near-annual Assembly resolutions, communiqués, interpretive notes, anniversary restatements — re-performs positions without altering anyone's obligations; functional activity (negotiation over actual terms) is the minority share. Accessibility collapse is moderate (0.45): exits exist and have been used (Egypt and Jordan settled bilaterally; parties resort to emergency special sessions and unilateral declarations), but none delivers general closure, so alternatives degrade without collapsing. Resistance is high (0.70): jurisdiction refusals, vetoes, rival resolutions, and scholarly contestation are the arrangement's normal operating condition, not anomalies. The claimed_type (snare) is authored independently from the structural analysis — identifiable victims, a thin coordination cover story, and persistence dependent on active enforcement — while the metrics are authored descriptively; the engine computes per-seat classifications and any divergence between claim and computation is the datum. All three temporal series run on one shared eight-point grid (t0=1967 adoption; t6≈1973 war and Resolution 338; t12≈1979 Egypt–Israel treaty; t24≈1991 Madrid/1993 Oslo; t37≈2004 wall advisory opinion; t45≈2012 Assembly observer status; t51≈2018–19 recognition moves; t57≈2024 occupation-legality advisory opinion), with end-state values matching the base_properties scalars. The trajectories show accumulation rather than cycle: extraction and theater rise monotonically as ambiguity hardens from drafting accident into strategic asset, and suppression plateaus after 2004 as the non-cooperation doctrine reaches maturity — the post-advisory-opinion phase is a compliance fight, not a new escalation.
 *
 * PERSPECTIVAL GAP:
 *   From the veto-holding seats the arrangement is sovereignty-preserving pluralism: no tribunal should bind a non-consenting state, and keeping interpretation distributed is fidelity to the consent principle, not obstruction. From the closure-seeking seats the same structure is engineered indefiniteness — a machine that converts a legal question into forever-diplomacy, with the bill paid by people under administration and in exile. From the Court's seat it is institutional frustration: authority exercised when seised, acceptance withheld by exactly the actors whose consent would matter. From the displaced populations' seat it is simply the reason the condition persists. The engine computes these divergent per-seat classifications from the authored power, exit, and role data; the story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the three beneficiary seats (veto holders, administering state, drafting states) derive low directionality — nearest the beneficiary end for the arbitrage-exit seats (veto holders, drafting states), slightly higher for the administering state, whose exit is constrained and who bears recurring condemnation, isolation, and open-ended security-administration costs alongside its positional gain. The three victim seats derive high directionality: the displaced populations (powerless, trapped) sit nearest the full-target end; the frontline states and the Assembly majority (organized, constrained) sit high but below them, since both retain partial exits through bilateral settlement and coalition diplomacy. No directionality_overrides are authored: the derivation chain captures the structure from the declarations plus exit data, and the one genuinely mixed seat — the Court, which gains docket and precedence from the dispute but whose institutional interest lies in its interpretations being accepted — shares its power atom (institutional) with two beneficiaries, and since override entries key on the power atom rather than the agent, an override would misfire across all three seats; the Court's mixed stake is left to the structural derivation and flagged here. On the receipt surface: gain_flow names the occupying_state because the concrete, continuous good the arrangement defers — administrative control of the territory — lands directly and exclusively on that seat, while the veto holders' gains (preserved discretion, precedent value) are conditional and issue-diffuse; receipt is narrower than benefit, and the distinction is deliberate. fixing_cost is prohibitive: forcing definitive interpretation requires either unanimity among the very seats that profit from its absence or compelling compliance by a state in physical control of the territory — either path costs more than any single seat gains from closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — producing a text all permanent members could adopt in November 1967 — was solved at adoption and is dead; what persists is the arrangement the solution left behind, maintained because the seats that profit from persistence hold the levers. The founding_problem_status x disappearance_verdict pair (dead + world_rearranges) is the designed mismatch signature: a mandate outlived by its arrangement, kept alive by capture rather than function. The classification guards against two opposite mislabels. Reading the arrangement as rope would credit the cover story — Resolution 242 genuinely remains the one text all parties cite, and that shared vocabulary has real value — while ignoring that the coordination residue is thin and the asymmetry total. Reading it as mere absence of law would miss that the vacuum is actively produced: vetoes are cast, consent is refused, advisory outputs are publicly repudiated — enforcement expenditure on behalf of nobody-decides. Snare captures both halves: a genuine but minor coordination function operating as the vehicle for a durable, enforced transfer of settlement-deferral benefit from the closure-seeking to the veto-holding and administering seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the interpretive_authority_structure reading of the unsc_242_withdrawal_clause kernel; how would the classification change under the substantive sibling readings?',
    'Author maximal_withdrawal_reading and partial_withdrawal_reading as separate constraint stories and compare beneficiary/victim sets, epsilon, and computed types across the family.',
    'Under the maximal reading the administering state becomes a straightforward violator and closure-seeking parties gain standing; under the partial reading territorial retention becomes lawful and the Court''s role contracts — both siblings would relocate this story''s victims and rewrite its transfer function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one of three readings of a contested kernel; sibling stories carry the substantive-scope contests.').

omega_variable(
    consent_rule_vs_effective_control,
    'Does interpretive authority in this system ultimately follow doctrinal entitlement (judicial office, authorship of the text) or effective control on the ground?',
    'Longitudinal tracking of which interpretive claims are cited in subsequent treaties, national court decisions, and acknowledged state practice.',
    'If effective control wins, the administering state''s position hardens into accepted custom and the tri-polar contest collapses in its favor; if doctrine wins, authority consolidates toward the Court and the standing arrangement unwinds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_rule_vs_effective_control, empirical, 'Whether authority tracks office and consent or facts on the ground.').

omega_variable(
    suppression_structural_or_identity_fused,
    'Is the blocking of definitive resolution purely structural (veto arithmetic, consent-based jurisdiction) or partly internalized (parties'' security and national doctrines now constituted by their interpretive positions)?',
    'Counterfactual test: if veto arithmetic changed — a P5 consensus referral to a binding forum — would the administering state comply, or would non-cooperation persist independent of the structural gate?',
    'If internalized, removing the structural gate would not produce closure; suppression would migrate into a compliance-fighting phase and the post-gate arrangement would classify differently than a purely structural account predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_identity_fused, empirical, 'Structural versus internalized maintenance of the interpretive deadlock.').

omega_variable(
    advisory_opinion_accumulation,
    'Will successive advisory opinions (2004 wall opinion, 2024 occupation-legality opinion) accumulate into generally accepted judicial authority, converting the distributed interpretive structure into a Court-centered one?',
    'Track citation uptake: national court references, General Assembly follow-through, treaty preambles, and patterns of state protest or acquiescence.',
    'Accumulation would date the beginning of the end of this constraint; persistent rejection by the same blocs would confirm its stability and the adequacy of the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_opinion_accumulation, empirical, 'Whether episodic exercises of claimed judicial authority are accreting into settled supremacy.').

omega_variable(
    unanimity_purchase_reversibility,
    'Was the article-level ambiguity a one-time price paid for 1967 unanimity, or is it continuously reproduced by current incentives such that even a redrafting conference would re-ambiguate?',
    'Examine attempted clarifications (Camp David formulations, Oslo-era language, the Arab Peace Initiative wording) for whether each party re-insists on its own article whenever closure approaches.',
    'If continuously reproduced, the arrangement is self-maintaining rather than inherited — persistence predictions and any intervention design must target current incentives, not the 1967 drafting event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_purchase_reversibility, conceptual, 'Whether the ambiguity is a legacy artifact or a live equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_interp_auth_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t0, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t6, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t6, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t12, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t12, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t24, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 37, 0.5).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t37, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 45, 0.53).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t45, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t51, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 51, 0.56).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t51, observed).
narrative_ontology:measurement(unsc242_interp_auth_tr_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 57, 0.58).
narrative_ontology:measurement_basis(unsc242_interp_auth_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(unsc242_interp_auth_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t0, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t6, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t6, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t12, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t12, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t24, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 37, 0.7).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t37, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 45, 0.73).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t45, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t51, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 51, 0.76).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t51, observed).
narrative_ontology:measurement(unsc242_interp_auth_be_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 57, 0.78).
narrative_ontology:measurement_basis(unsc242_interp_auth_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_interp_auth_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t0, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t6, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t6, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t12, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t12, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t24, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 37, 0.59).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t37, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 45, 0.61).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t45, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t51, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 51, 0.62).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t51, observed).
narrative_ontology:measurement(unsc242_interp_auth_su_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 57, 0.62).
narrative_ontology:measurement_basis(unsc242_interp_auth_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, information_standard).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the unsc_242_withdrawal_clause kernel, decomposed per the ε-invariance principle: the colloquial label 'the withdrawal clause' covers three structurally distinct constraints. Two are substantive-scope claims (maximal_withdrawal_reading: withdrawal mandatory from all territories; partial_withdrawal_reading: scope discretionary per drafters' intent) with different victim sets, different beneficiary sets, and different ε. The third — this story — is the meta-level authority-allocation arrangement whose ε is high for a different reason: authority ambiguity perpetuates substantive ambiguity. Upstream/downstream structure runs from this reading to both siblings: its operation determines which substantive reading can achieve settled status, so its edges (influences) condition the siblings' legitimacy environment without foreclosing either. Each family member links to the others via network.affects_constraints; no member is evaluable in isolation, because each sibling's persistence probability is a function of this arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
