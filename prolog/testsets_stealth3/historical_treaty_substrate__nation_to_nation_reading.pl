% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Reading of the Historical Treaty Substrate
 *   domain: legal anthropology / indigenous law / comparative constitutional theory
 *
 * SUMMARY:
 *   Historical treaties between settler states and Indigenous nations are
 *   here construed as live international agreements between sovereign equals:
 *   instruments that required — and on this reading still require — the
 *   ongoing consent of both parties, subject to modern treaty-law principles
 *   such as pacta sunt servanda. The standing arrangement this story assesses
 *   is the actual operation of that substrate over the modern era: settler
 *   states administer the compacts through domestic law, permit unilateral
 *   resource development on treaty lands, and resist the internationalization
 *   of treaty obligations, while Indigenous nations assert consent rights
 *   through litigation, blockade, and international advocacy. Epsilon is
 *   authored for THAT standing arrangement as this reading sees it — high,
 *   because the arrangement delivers territorial value and jurisdictional
 *   control to the settler side without the consent its own frame requires.
 *   This file instantiates ONE reading of the kernel
 *   historical_treaty_substrate; the extinguishment reading (treaties as
 *   completed property transactions) and the stewardship reading (relational
 *   pacts without cession) are separate constraints with their own epsilon
 *   values, linked through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - settler_state_governments: Agenda-setting
 *   administrator (institutional/arbitrage) — controls interpretation,
 *   enforcement, and the terms on which obligations are honored -
 *   indigenous_nations: Primary target (organized/trapped) — bears the
 *   extraction; asserts the consent rights this reading recognizes -
 *   resource_extraction_industries: Secondary beneficiary (powerful/mobile) —
 *   works treaty lands under state permits - settler_civilian_populations:
 *   Diffuse beneficiary (organized/constrained) — occupies treaty lands and
 *   consumes their yields - oral_tradition_keepers: Excluded voice
 *   (moderate/identity_locked) — holds the Indigenous half of treaty meaning,
 *   shut out of state-side interpretation - international_legal_bodies:
 *   Analytical observer (institutional/analytical) — reviews state conduct,
 *   lacks enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.64).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.73).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Reading of the Historical Treaty Substrate").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal anthropology / indigenous law / comparative constitutional theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '44677016-1afb-4725-afed-81b4d1cd1e3c').
narrative_ontology:cs_kernel_codification('44677016-1afb-4725-afed-81b4d1cd1e3c', fixed_text).
narrative_ontology:cs_authority_grounding('44677016-1afb-4725-afed-81b4d1cd1e3c', lineage).
narrative_ontology:cs_interpretation_layer_present('44677016-1afb-4725-afed-81b4d1cd1e3c').
narrative_ontology:cs_reading_relation('44677016-1afb-4725-afed-81b4d1cd1e3c', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('44677016-1afb-4725-afed-81b4d1cd1e3c', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('44677016-1afb-4725-afed-81b4d1cd1e3c', foundational, treaty_parties_retain_inherent_sovereignty).
narrative_ontology:cs_axiom_status(treaty_parties_retain_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('44677016-1afb-4725-afed-81b4d1cd1e3c', treaty_parties_retain_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('44677016-1afb-4725-afed-81b4d1cd1e3c', secondary, treaty_obligations_bind_across_generations).
narrative_ontology:cs_axiom_status(treaty_obligations_bind_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('44677016-1afb-4725-afed-81b4d1cd1e3c', treaty_obligations_bind_across_generations, conventional).
narrative_ontology:cs_reference_frame('44677016-1afb-4725-afed-81b4d1cd1e3c', sovereign_equals_treaty_framework).
narrative_ontology:cs_drift_state('44677016-1afb-4725-afed-81b4d1cd1e3c', contemporary_post_undrip_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44677016-1afb-4725-afed-81b4d1cd1e3c', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_civilian_populations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, oral_tradition_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the treaty relationship through domestic law: legislatures pass statutes governing treaty lands, courts interpret the treaty texts and decide which obligations remain operative, agencies permit resource development and distribute annuities. They choose which understanding of the compacts governs day to day, fund the litigation that defends their interpretations, and face international review they do not control. Honoring the full set of inherited obligations would require renegotiating, with the other treaty parties, terms they currently administer unilaterally.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold the other end of the compacts: they provided territorial access and alliance under written and orally transmitted commitments they understand as continuing. They live with development undertaken on their territories without their agreement, carry the legal costs of asserting their reading in domestic and international forums, and cannot exit the relationship — their territories, memberships, and governance are constituted inside it. Some nations have secured modern agreements restoring partial decision-making authority; most continue to assert consent rights the state does not recognize.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer,
    organized, generational, trapped, continental).

% Operate mines, pipelines, forestry concessions, and hydroelectric projects on treaty lands under state-issued permits. They pay royalties and fees to the state rather than consent-based terms to the nations whose territories they work, and can relocate capital to new jurisdictions when local contestation raises their costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries, beneficiary,
    powerful, biographical, mobile, global).

% Occupy and farm treaty lands, draw municipal water and power from infrastructure built on them, and receive services financed in part by treaty-land revenues. They also bear diffuse costs: tax-funded litigation and settlements, and periodic disruption from blockades and protests — and they vote for the governments that set the state's reading of the compacts.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_civilian_populations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_civilian_populations, payer).

% Carry the Indigenous side of treaty meaning — wampum belts, pipe ceremonies, recited terms of alliance — that the written colonial record does not contain. Domestic courts admit their testimony only sporadically, and state-side interpretation proceeds from minutes and dispatches produced by the state's own negotiators. They hold that the compacts recorded mutual aid and shared use rather than surrender, and that their account is the surviving half of each agreement. Their office is constituted by the recitation itself; setting it aside would mean abandoning the tradition they are.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, oral_tradition_keepers, excluded,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, oral_tradition_keepers, payer).

% United Nations treaty bodies, special rapporteurs, and regional human-rights mechanisms review state conduct against international standards, issue observations, and adopt instruments such as the declaration on Indigenous rights. They hold no enforcement power over the states concerned; their findings shape reputational costs and supply arguments to domestic litigants.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The compacts solved a real problem: ordering durable relations between polities sharing the same territory — boundaries, alliance, trade, mutual defense, and jurisdiction — without perpetual war. Stated without evaluation: they established a working framework for coexistence between two sets of sovereign peoples.
% TRANSFER_FUNCTION: Moves territorial access, resource rights, and jurisdictional control from Indigenous nations to the settler state and its licensed industries; moves annuities, services, and formal recognition back toward the nations — in the standing arrangement, at a fraction of the value flowing the other way.
% ABSENT_VOICES: Oral tradition keepers are structurally excluded from state-side interpretation of what the compacts say; the original signatories' intended terms survive mainly in accounts the state's courts do not treat as authoritative. Future generations of both parties are absent whenever consent is presumed discharged. International adjudicators are kept out by state refusal of jurisdiction over the disputes.
% DISAPPEARANCE_RATIONALE: If the treaty substrate and its enforcement vanished overnight, the territorial and jurisdictional order of the settler states would lose its legal foundation: land titles, provincial and state boundaries, resource permitting, and the whole apparatus of domesticated treaty law rest on the compacts' continued (if contested) operation. Every seat in this story is positioned by the arrangement's existence.
% FOUNDING_PROBLEM: The compacts were built to solve the problem of peaceful ordered coexistence between expanding settler polities and Indigenous nations: ending cycles of war, fixing boundaries and trade, securing alliance, and establishing who governed what.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: Indigenous nations' own oral traditions and contemporary governments attest both the founding problem and its persistence (they bear the arrangement's costs, not its gains); the UN General Assembly's adoption of the declaration on Indigenous rights and recurring treaty-body reviews attest that sovereign-consent standards remain live; royal commissions and truth-and-reconciliation inquiries document the founding problem's history and unresolved status. The settler state — the benefiting party — disputes this reading; no source within the beneficiary set attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 (down from 0.78 at interval start): under this reading the standing arrangement transfers territorial and resource value without the ongoing consent it requires, but the measured rate has declined slowly as international norms spread — modern agreements, compensation frameworks, and consent language have bought down the headline rate without restructuring the flow. Suppression is higher (0.73) and rising: the enforcement requirement has matured from overt policing of assertion into doctrinal management — courts domesticating the compacts into municipal law, duty-to-consult frameworks absorbing dissent into procedure — so keeping the international reading suppressed now takes more sophisticated machinery, not less. Theater_ratio rises from 0.22 to 0.60, crossing the substitution threshold mid-interval: land acknowledgments, commemoration, and consultation-without-consent-authority increasingly stand in for the substantive function (classic Goodhart drift, and the reason the extraction decline should not be read as resolution). Accessibility_collapse is 0.55: domestic courts closed off the international-law reading early and thoroughly, leaving alternatives that exist but are costly and weakly enforceable. Resistance is 0.66: sustained litigation, blockades, and international advocacy meet the arrangement continuously. Claim/metric independence is preserved: the claimed type (tangled_rope) reflects my structural judgment that the substrate retains a genuine coordination function while its current operation extracts asymmetrically under active enforcement; the metrics describe the operation as I find it, without tuning toward any predicted engine verdict. All three temporal series share one grid (points 0-60 step 10) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute differently from the same instruments. From the indigenous_nations seat the arrangement operates as enforced extraction wearing ceremonial recognition: obligations asserted as live, development proceeding without agreement, recourse channeled into forums the state controls. From the settler_state_governments seat the same structure presents as a managed legal regime inherited in good faith — treaties honored 'within the constitutional framework,' consultation conducted, annuities paid — with the international frame experienced as an external attack on settled law rather than as the compacts' own terms. The industry seat experiences ordinary permitting; the excluded keeper seat experiences silencing. Identity-lock dynamics concentrate in the oral_tradition_keepers seat: the lock is professional-relational — the office exists only through continued recitation, so exit would dissolve the identity rather than escape the constraint; if that frame broke (if written and oral records were legally merged), the excluded voice would convert into a competing interpretive authority and the state's interpretive monopoly would fracture. The indigenous_nations seat is trapped rather than identity_locked: exit is structurally impossible (territory, membership, governance), which is a different mechanism from fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly onto the flow of value. settler_state_governments sit nearest the beneficiary end: they capture the territorial yield and the avoided cost of consent, moderated somewhat by the enforcement, litigation, and reputational expenses they bear — they are the capturer, not a pure free-rider. resource_extraction_industries derive low directionality (benefit, mobile capital). settler_civilian_populations sit low-to-moderate: diffuse benefit from occupation and revenues, diffuse cost as taxpayers. indigenous_nations sit near the full-target end — victim declarations plus trapped exit amplify effective extraction toward its maximum; their organized power raises resistance but does not dampen what flows from them. oral_tradition_keepers carry high directionality through their payer secondary role: the arrangement's interpretive suppression falls on them directly. international_legal_bodies are analytically neutral. Note on scaling: suppression is authored as a raw structural property and is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled, by directionality and spatial scope — the state's national scope and the industries' global reach modestly amplify the verification difficulty baked into effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ordering coexistence between peoples sharing territory — remains live, so this is not a mandatrophy case and mandatrophy_resolved is deliberately not declared. The classification discipline cuts both ways here. Reading the substrate as pure extraction would erase a real coordination achievement: the compacts did substitute ordered coexistence for open war across centuries, and remedies premised on 'the whole thing is a con' misdirect toward demolition when the live work is renegotiation. Reading it as pure coordination would excuse the standing arrangement's asymmetric transfer — the annuity-for-territory exchange at rates set unilaterally by one party. The tangled_rope structure keeps both truths load-bearing: the coordination function explains why the instruments persist and why Indigenous nations litigate to enforce them rather than abandon them; the extraction explains why enforcement of the reading, not celebration of the compacts, is the operative demand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is one reading of the kernel historical_treaty_substrate — how would the classification change under the sibling readings?',
    'Compare the compiled classifications of extinguishment_reading and stewardship_reading against this story: the same metric surface read through different beneficiary/victim structures and violation standards.',
    'Under extinguishment_reading the indigenous nations drop out of the beneficiary-relevant structure entirely and become compensated sellers, collapsing the consent-rights architecture this reading asserts and driving measured extraction toward near zero; under stewardship_reading the contractual consent mechanism is replaced by relational obligation, changing which enforcement facts count as violations and likely lowering the theater reading (no consent ritual to substitute for).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: sibling readings would restructure the beneficiary/victim sets and the violation standard over the same referent.').

omega_variable(
    consultation_absorption_or_recognition,
    'Does the duty-to-consult machinery constitute partial recognition of consent rights, or absorption theater that converts assertion into procedure?',
    'Track outcomes: whether consultation processes have ever halted or materially rerouted major projects in the absence of Indigenous agreement, versus approving after process completion regardless of the position taken.',
    'If recognition, effective extraction is lower than measured and the trajectory bends toward a transitional arrangement with a real sunset path; if absorption, theater_ratio is understated and the enforcement ratchet is stronger than the suppression series shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_absorption_or_recognition, empirical, 'Whether consultation regimes recognize consent or metabolize dissent.').

omega_variable(
    international_recourse_binding_force,
    'Can international mechanisms (declaration-review bodies, treaty-body findings, supervisory procedures) actually constrain settler-state conduct, or do they generate only reputational cost?',
    'Compare project trajectories where international bodies flagged violations against matched unflagged projects; measure whether findings alter financing, insurance, or permitting decisions.',
    'If binding in effect, suppression will decay as domestic assertion gains leverage and the constraint trends toward renegotiation; if purely reputational, the suppression series understates how closed the international exit actually is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recourse_binding_force, empirical, 'Effectiveness of the international avenue this reading relies upon.').

omega_variable(
    interpretive_authority_location,
    'Whose interpretation of treaty meaning governs — the written colonial record held in state archives, or the oral traditions held by the nations'' knowledge keepers?',
    'Legal developments admitting oral tradition as authoritative evidence (as some jurisdictions have begun to), and archival comparison of written minutes against recited terms where both survive.',
    'If oral authority gains standing, the agenda-setting seat fractures and the state''s interpretive monopoly — a mainstay of the measured suppression — erodes; if not, the excluded voice remains structurally silenced and the theater component persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_location, conceptual, 'Location of interpretive authority over the kernel texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 50, 0.54).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 60, 0.6).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 60, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 60, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'historical treaties' decomposes into three structurally distinct constraints, one per reading of the kernel. Epsilon differs sharply across the family: the extinguishment reading authors low residual extraction (transactions complete, obligations discharged); this reading authors high extraction (obligations live and violated); the stewardship reading authors extraction measured against relational-obligation defaults rather than contractual consent. Structural linkage: the extinguishment reading is the settler state's operational default and supplies the domestic legal precedents this reading must displace; this reading supplies the international-law pressure that reshapes the stewardship reading's operating environment (consent-language instruments give stewardship claims legal hooks). Each family member links the others via network.affects_constraints; no member is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
