% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Reading of Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the Palestinian autochthony reading of the
 *   contested territorial-legitimacy kernel: legitimacy is grounded in
 *   continuous pre-1948 habitation, the 1948 displacement (Nakba) is read as
 *   an ongoing, unremedied injustice rather than a settled historical fact,
 *   subsequent territorial reduction (1967 occupation, ongoing settlement
 *   expansion) is read as compounding deprivation, Israeli state legitimacy
 *   over the disputed territory is contested rather than accepted, and the
 *   right of return under UNGA 194 is treated as non-negotiable rather than
 *   as a bargaining chip. This is ONE of three readings of the same kernel
 *   (territorial_legitimacy_dual); the zionist_refuge_reading and
 *   two_state_coexistence_reading are separate constraint stories with their
 *   own epsilon values, beneficiary/victim structures, and classifications.
 *   This story does not average across readings or hedge between them — it
 *   authors the Palestinian autochthony reading cleanly, on its own terms, as
 *   the reading's own lights assess the standing arrangement (the current
 *   territorial and citizenship regime), not the reading's endorsed remedy
 *   (full return and restitution, which would make epsilon near zero from
 *   this same seat).
 *
 * KEY AGENTS:
 *   - palestinian_refugees_1948: primary target (powerless/trapped) — bears the founding displacement and its unremedied continuation
 *   - palestinian_residents_occupied_territories: primary target (powerless/trapped) — bears ongoing territorial reduction and movement restriction
 *   - palestinian_diaspora_right_of_return_claimants: secondary target/excluded (moderate/constrained) — bears exclusion from negotiation despite mobility
 *   - israeli_state_apparatus: primary beneficiary/agenda-setter (institutional/arbitrage) — administers the citizenship and land regime this reading identifies as the extraction mechanism
 *   - israeli_settler_population: secondary beneficiary (organized/mobile) — occupies land and receives subsidy under the contested arrangement
 *   - international_mediating_powers: analytical observer (institutional/analytical) — shapes which reading of the kernel a given negotiation privileges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.81).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.86).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '015a2ad2-4990-41e8-a024-31e0150cce88').
narrative_ontology:cs_kernel_codification('015a2ad2-4990-41e8-a024-31e0150cce88', distributed).
narrative_ontology:cs_authority_grounding('015a2ad2-4990-41e8-a024-31e0150cce88', distributed).
narrative_ontology:cs_reading_relation('015a2ad2-4990-41e8-a024-31e0150cce88', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('015a2ad2-4990-41e8-a024-31e0150cce88', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('015a2ad2-4990-41e8-a024-31e0150cce88', foundational, displacement_creates_non_extinguishable_claim).
narrative_ontology:cs_axiom_status(displacement_creates_non_extinguishable_claim, holdable).
narrative_ontology:cs_axiom_grounding('015a2ad2-4990-41e8-a024-31e0150cce88', displacement_creates_non_extinguishable_claim, deontological).
narrative_ontology:cs_axiom('015a2ad2-4990-41e8-a024-31e0150cce88', foundational, continuous_habitation_grounds_primary_title).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_primary_title, holdable).
narrative_ontology:cs_axiom_grounding('015a2ad2-4990-41e8-a024-31e0150cce88', continuous_habitation_grounds_primary_title, deontological).
narrative_ontology:cs_axiom('015a2ad2-4990-41e8-a024-31e0150cce88', secondary, return_is_non_negotiable_precondition).
narrative_ontology:cs_axiom_status(return_is_non_negotiable_precondition, holdable).
narrative_ontology:cs_axiom_grounding('015a2ad2-4990-41e8-a024-31e0150cce88', return_is_non_negotiable_precondition, conventional).
narrative_ontology:cs_reference_frame('015a2ad2-4990-41e8-a024-31e0150cce88', pre_1948_continuous_habitation_baseline).
narrative_ontology:cs_drift_state('015a2ad2-4990-41e8-a024-31e0150cce88', post_oslo_negotiation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('015a2ad2-4990-41e8-a024-31e0150cce88', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_population).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_right_of_return_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced from villages and towns in 1948 (the Nakba) and their multi-generational descendants, most in refugee camps or diaspora, hold UNRWA registration and title/deed records to land now inside Israel. They are barred by Israeli law from return or property restitution. Their claim rests on continuous habitation predating 1948 and on displacement as an unremedied wrong; exit from this constraint would require either forced naturalization elsewhere (erasing the claim) or an enforced right of return they have no power to compel.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under occupation or blockade in the West Bank and Gaza, subject to movement restriction, settlement expansion onto land they hold customary or title claims to, and military administration. They bear the ongoing territorial-reduction dimension of the same displacement logic that produced the 1948 refugees. Their exit options are foreclosed by checkpoints, permit regimes, and the absence of sovereign passport authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories, payer,
    powerless, generational, trapped, regional).

% Palestinians and their descendants living outside historic Palestine who hold UN General Assembly Resolution 194-grounded return claims. Some hold foreign citizenship and professional mobility (moderate power) but are structurally barred from the one form of return this reading demands as non-negotiable. They advocate internationally but are excluded from direct negotiation tables that have historically bracketed return as a 'final status' issue rather than a precondition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_right_of_return_claimants, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_right_of_return_claimants, excluded).

% Administers citizenship law, the Law of Return (for Jewish immigrants only), land registries, and military control over the West Bank, functionally foreclosing Palestinian return and adjudicating property claims through domestic courts that this reading holds structurally incapable of remedying 1948 dispossession. Sets the enforcement terms — border control, permit regimes, settlement policy — that this reading identifies as the ongoing mechanism of displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Resides on land in the West Bank and within pre-1967 Israel that this reading identifies as subject to Palestinian habitation claims. Benefits from state-subsidized housing, security infrastructure, and land allocation built on the territorial arrangement this reading contests. Retains full Israeli citizenship mobility and could relocate within Israel proper without loss of legal status — a degree of exit unavailable to any Palestinian claimant seat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_population, beneficiary,
    organized, generational, mobile, national).

% The UN, US, EU, and Arab League states that broker negotiations, fund UNRWA, and periodically restate positions on the right of return and territorial boundaries. They observe and sometimes fund both sides' claims without being bound as parties, and their diplomatic posture shapes which reading of the kernel a given negotiation round privileges.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_mediating_powers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination function this reading identifies in the standing arrangement — from this seat, the current territorial and citizenship regime does not solve a shared problem for Palestinians and Israelis jointly; it resolves the Israeli state's demographic and security objectives at Palestinian expense. The only 'coordination' this reading recognizes as legitimate is a future arrangement built on remedy, not the status quo.
% TRANSFER_FUNCTION: The arrangement moves land, residency rights, and citizenship-conferred security from pre-1948 Palestinian inhabitants and their descendants to the Israeli state and settler population, and continues moving land and movement rights from present-day occupied-territory residents to settlement infrastructure.
% ABSENT_VOICES: Palestinian refugees and diaspora claimants are structurally absent from final-status negotiations, where return has repeatedly been treated as a bargaining chip rather than a precondition; Gaza and West Bank residents under closure have no direct representation in negotiations conducted on their behalf by parties (PA, external mediators) whose interests do not fully align with theirs.
% DISAPPEARANCE_RATIONALE: If the current territorial-legitimacy arrangement (citizenship law, land registry, occupation administration) disappeared overnight, refugee return claims would become immediately actionable, settlement land tenure would become contestable, and the entire security-and-demographic architecture the Israeli state has built since 1948 would require renegotiation from the ground up — this is not a natural fact but a constructed, actively defended arrangement.
% FOUNDING_PROBLEM: From this reading's vantage, the arrangement was built to consolidate a Jewish-majority state on land with a pre-existing Palestinian population, requiring displacement (1948) and continued territorial control (1967 onward) to be treated as settled rather than as an ongoing wrong requiring remedy.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly Resolution 194 (1948) and subsequent UN bodies affirm the right of return as unresolved; Israeli New Historians (Benny Morris, Ilan Pappé, Avi Shlaim), writing from within Israeli academia rather than from Palestinian advocacy, corroborate the historical displacement record using Israeli state archives. This is corroboration from outside the group this reading identifies as the beneficiary, though it does not resolve the contested normative question of remedy.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because, from this reading, the arrangement's core function is transferring land, residency, and security from a displaced and continuously-dispossessed population to a state and settler population that administers the exclusion. Suppression is authored even higher (0.86) because persistence of the arrangement depends on active enforcement — citizenship law, military administration, permit regimes, border control — not on Palestinian consent or preference; suppression is a raw structural fact, unscaled by power or scope per the framework's rule, and it is high here regardless of which claimant seat is examined. Theater ratio is moderate (0.32, peaking near the Oslo period at 0.40) reflecting periods where negotiation processes performed movement toward resolution without altering the underlying displacement and land-transfer mechanics — Oslo-era theater cooling structural enforcement metrics temporarily before both climbed again after 2000. Accessibility collapse is moderate (0.62) rather than near-total because international legal instruments (UNGA 194, ICJ advisory opinions) keep an alternative framework alive even though it is not domestically enforceable. Resistance is very high (0.88) reflecting sustained Palestinian political, diplomatic, and periodic armed resistance to the arrangement across the full interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugee, occupied-territory, and diaspora seats are declared victims: displacement and territorial reduction are the mechanisms by which the arrangement extracts land and security, so directionality for these seats sits near the full-target end regardless of nominal citizenship status elsewhere, because their claim to THIS territory is what is foreclosed. The Israeli state apparatus and settler population are declared beneficiaries: they administer and occupy the land whose transfer constitutes the extraction, so directionality sits near the full-beneficiary end. International mediators are analytical, not parties to the transfer, so they sit outside the beneficiary/victim axis entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy misclassification in a specific direction: because the founding displacement (1948) is treated by many institutional accounts as historically settled even if regrettable, there is a risk of reading the current arrangement as a Piton (a vestigial, inert structure) rather than as an actively enforced Snare. The measurement series is authored to show suppression_requirement remaining high and even rising through 2024, precisely to block that misreading — this is not vestigial inertia but active, evolving enforcement (permit regimes, settlement expansion, blockade administration) that intensifies rather than atrophies. The founding_problem_status is authored 'live' rather than 'dead' for the same reason: from this reading, the problem the arrangement addresses (Israeli demographic and territorial security amid a Palestinian claimant population) remains actively managed, not resolved, and the diffusion of that management across decades should not be mistaken for the mandate itself dissolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_remedy_versus_negotiated_compromise,
    'Does justice for 1948 displacement require literal right of return and restitution (this reading''s position), or can it be satisfied through negotiated compensation, resettlement assistance, and symbolic acknowledgment (the two_state_coexistence_reading''s position)?',
    'No empirical resolution exists; this is a normative/political question ultimately settled (if ever) through negotiated agreement between claimant parties or through international legal adjudication with enforcement power, neither of which currently exists.',
    'If literal return is accepted as the only legitimate remedy, the current arrangement remains a snare for as long as return is foreclosed. If negotiated compromise is accepted as sufficient remedy, the classification could shift toward tangled_rope once a compensation/coexistence framework is actually implemented and enforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_remedy_versus_negotiated_compromise, preference, 'Whether remedy for displacement must be literal return or can be satisfied by negotiated compromise — the central normative fork between this reading and the coexistence reading.').

omega_variable(
    state_legitimacy_contestation_scope,
    'Is Israeli state legitimacy contested only with respect to the specific territories acquired through 1948 displacement and 1967 occupation, or does this reading extend to contesting the legitimacy of the state''s existence within any borders?',
    'Textual and political analysis of how this reading''s proponents (PLO charter history, contemporary Palestinian political factions, BDS movement framing) have evolved on this question, which has shifted significantly since the 1990s Oslo-era recognition of Israel''s existence within some borders.',
    'A narrower reading (contesting only post-1948/1967 territorial legitimacy) supports classification closer to tangled_rope with a negotiated remedy path; a maximalist reading (contesting the state''s existence in any form) forecloses negotiated coexistence entirely and supports a harder snare classification with no tangled coordination function at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_contestation_scope, conceptual, 'Whether this reading contests territorial scope specifically or state legitimacy as such — determines whether any negotiated remedy is structurally available.').

omega_variable(
    corroboration_asymmetry,
    'Israeli New Historian scholarship corroborates the historical displacement record but not necessarily the normative non-negotiability of return — does partial corroboration on facts without corroboration on remedy still count as outside corroboration for this reading''s full claim?',
    'Distinguish empirical corroboration (what happened in 1948) from normative corroboration (what remedy is owed) in future citation of this reading; track whether outside scholarship or legal bodies affirm the non-negotiability claim specifically, not just the historical record.',
    'If only the empirical layer is corroborated from outside the beneficiary group, the normative claim (non-negotiable return) remains self-asserted by the claimant population itself, which is weaker corroboration than the story''s founding_problem_corroboration field implies at first read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_asymmetry, conceptual, 'Whether outside corroboration of historical facts extends to corroboration of this reading''s normative remedy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement_basis(terr_tr_t2010, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.72).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement_basis(terr_be_t2010, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement_basis(terr_su_t2010, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.86).
narrative_ontology:measurement_basis(terr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the territorial_legitimacy_dual kernel. Each reading authors its own epsilon, beneficiary/victim structure, and claimed_type from its own normative starting point, applied to the same referent (the standing territorial/citizenship arrangement), per the epsilon-invariance principle for kernel-reading stories. The palestinian_autochthony_reading (this story) authors high extraction/suppression and claims snare; the zionist_refuge_reading is expected to author low-to-moderate extraction from a defensive/refuge framing; the two_state_coexistence_reading is expected to author moderate extraction with an explicit remedy/sunset path, potentially approaching scaffold or tangled_rope. All three link to each other via affects_constraints rather than being merged into one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
