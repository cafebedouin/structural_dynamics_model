% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Stewardship Reading
 *   domain: legal_anthropology/indigenous_law/constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the stewardship_reading of the
 *   historical_treaty_substrate kernel. The reading interprets historical
 *   treaties (e.g., Numbered Treaties, Peace and Friendship Treaties, Douglas
 *   Treaties) as relational pacts establishing ongoing shared stewardship of
 *   territory — not as cession of sovereignty. Indigenous nations remain
 *   beneficiaries of territorial jurisdiction; the settler state enters an
 *   obligation set for consent-based coexistence and joint governance;
 *   territorial resources are to be jointly managed rather than unilaterally
 *   extracted. The claim/metric independence is deliberate: the reading
 *   CLAIMS a rope-like mutual coordination (tangled_rope claimed), while the
 *   metrics reflect the historical and ongoing asymmetric extraction that the
 *   reading contests. The engine computes per-seat divergence from the
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.48).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.52).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '6a24796a-ae2b-4452-8de9-cdda7c89d830').
narrative_ontology:cs_kernel_codification('6a24796a-ae2b-4452-8de9-cdda7c89d830', formalized).
narrative_ontology:cs_authority_grounding('6a24796a-ae2b-4452-8de9-cdda7c89d830', lineage).
narrative_ontology:cs_interpretation_layer_present('6a24796a-ae2b-4452-8de9-cdda7c89d830').
narrative_ontology:cs_reading_relation('6a24796a-ae2b-4452-8de9-cdda7c89d830', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('6a24796a-ae2b-4452-8de9-cdda7c89d830', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('6a24796a-ae2b-4452-8de9-cdda7c89d830', foundational, no_cession_of_sovereignty).
narrative_ontology:cs_axiom_status(no_cession_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6a24796a-ae2b-4452-8de9-cdda7c89d830', no_cession_of_sovereignty, deontological).
narrative_ontology:cs_axiom('6a24796a-ae2b-4452-8de9-cdda7c89d830', foundational, mutual_stewardship_obligation).
narrative_ontology:cs_axiom_status(mutual_stewardship_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6a24796a-ae2b-4452-8de9-cdda7c89d830', mutual_stewardship_obligation, conventional).
narrative_ontology:cs_reference_frame('6a24796a-ae2b-4452-8de9-cdda7c89d830', original_treaty_relationship).
narrative_ontology:cs_drift_state('6a24796a-ae2b-4452-8de9-cdda7c89d830', contemporary_legal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a24796a-ae2b-4452-8de9-cdda7c89d830', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_citizens).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, resource_extraction_corporations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, inherent_indigenous_sovereignty).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, treaty_as_relational_pact).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, shared_stewardship_over_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold inherent sovereignty and territorial jurisdiction affirmed by treaties; bear historical and ongoing costs of land loss, resource extraction without consent, and cultural disruption; exit from treaty relationship is structurally unthinkable because territory and nationhood are fused — identity_locked by relational ontology, not mere preference.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_nations, payer).

% Administers treaty implementation through legislation, courts, and bureaucratic structures; benefits from treaty legitimacy for state sovereignty claims and resource access; constrained exit because treaty obligations are constitutionally entrenched and internationally monitored, but retains unilateral interpretation power in practice.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state, beneficiary).

% Benefit from stable coexistence, property certainty, and resource access secured by treaty relations; mobile exit (emigration) is legally possible but socially costly; not direct parties to treaty obligations but their political choices shape state compliance.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_citizens, beneficiary,
    organized, biographical, mobile, national).

% Gain access to territorial resources through state-granted licenses; arbitrage-grade exit (capital mobility, jurisdictional shopping); lobby for narrow treaty interpretations that minimize consent requirements and maximize extraction freedom.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Monitor treaty compliance through UNDRIP, CERD, HRC mechanisms; issue findings that create reputational and legal pressure on settler state; analytical seat with no direct stake in territorial distribution.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Indigenous peoples without historic treaties or whose treaties were never honored; would object to stewardship reading that centers treaty-holders while their territories remain unprotected; trapped by state refusal to negotiate or recognize rights.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, non_signatory_indigenous_groups, excluded,
    moderate, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a relational framework for shared territorial stewardship between sovereign Indigenous nations and the settler state, replacing conquest with mutual obligations of consent, coexistence, and joint decision-making over lands and resources.
% TRANSFER_FUNCTION: Moves unilateral decision-making authority over territorial resources from the settler state to a joint governance regime requiring Indigenous consent; moves the burden of justification from Indigenous nations (proving title) to the state (obtaining consent); transfers a share of resource revenues and management authority to Indigenous governments.
% ABSENT_VOICES: Non-signatory Indigenous nations whose territories were never treated; urban Indigenous populations disconnected from treaty organizations; future generations who inherit treaty obligations without having consented; ecosystems and non-human beings that are parties to Indigenous legal orders but invisible in state law.
% DISAPPEARANCE_RATIONALE: If treaty stewardship obligations vanished overnight, the legal basis for shared jurisdiction would collapse — the settler state would revert to unilateral control under doctrine of Crown sovereignty, Indigenous nations would lose constitutionally protected consultation and consent rights, resource extraction would accelerate without revenue sharing or environmental co-management, and the constitutional architecture of Canada (s.35) would lose its treaty foundation.
% FOUNDING_PROBLEM: The problem of establishing peaceful coexistence and shared territory between sovereign Indigenous nations and arriving settler populations without conquest — how to share land and governance while respecting each other's sovereignty and legal orders.
% FOUNDING_PROBLEM_CORROBORATION: Royal Commission on Aboriginal Peoples (1996) and Truth and Reconciliation Commission (2015) — arms-length state commissions — document that the founding problem of just coexistence remains unsolved. Supreme Court of Canada in Haida Nation (2004) and Tsilhqot'in Nation (2014) affirms the Crown's duty to consult and accommodate arises from the 'honour of the Crown' in treaty-making, not from a completed cession. International bodies (UN CERD, UNPFII) consistently find Canada's implementation falls short of treaty partnership. Indigenous legal scholars (Borrows, Napoleon, Craft, Asch) articulate the stewardship reading from within Indigenous legal orders. No credible non-Indigenous source corroborates that the founding problem is 'solved' — the settler state's own courts reject extinguishment.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness starts high (0.65 at 1763) reflecting immediate post-treaty land alienation and resource extraction without consent; peaks at 0.78 during peak settlement/railway era (1867-1917); declines after 1982 constitutional recognition (s.35) and modern treaty era to 0.48 — but remains non-zero because resource revenue sharing is partial and consent regimes are advisory not veto. Theater ratio rises from 0.15 to 0.41 as performative consultation replaces substantive consent after 1982. Suppression requirement falls from 0.85 (residential schools, pass system, Indian Act) to 0.48 (legal duty to consult) but rises slightly recently as enforcement of consultation becomes more procedural than substantive. All metrics share one time grid (0=1763 Royal Proclamation, 260=2024).
 *
 * PERSPECTIVAL GAP:
 *   From Indigenous nations' seat (identity_locked, organized power), the constraint is experienced as a living relational obligation the state dishonors — computed type trends toward snare. From settler state's seat (institutional, constrained exit), it is a manageable coordination framework with occasional friction — computed type trends toward rope. From resource corporations' seat (powerful, arbitrage), it is a regulatory cost to be minimized — computed type trends toward piton (theatrical compliance). The engine's per-seat computation captures this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are structural beneficiaries of territorial jurisdiction (d low) but also payers of historical extraction (d high) — the engine will compute a split directionality from dual role. Settler state is agenda_setter (administers implementation) and beneficiary (legitimacy, resources) — d near symmetric but tilted beneficiary. Resource corporations are pure beneficiaries with arbitrage exit — d near 0. Non-signatory Indigenous groups are excluded and trapped — their absence from the treaty frame is itself extractive. International bodies are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful coexistence without conquest) is contested as live — Indigenous nations say it persists; settler state says treaties resolved it. The mandate has not atrophied but has been actively subverted: the coordination function (shared stewardship) is displaced by extraction (unilateral development). Mandatrophy is not resolved — the arrangement persists in a zombified form where treaty language is retained but stewardship substance is hollowed out. This is not piton (inertial decay) but active contested transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the stewardship reading reflect the original treaty common understanding (as evidenced by Indigenous oral histories and negotiation records) or is it a contemporary reconstruction using modern legal concepts?',
    'Comparative analysis of treaty negotiation minutes, Indigenous oral testimony recorded at signing, and contemporaneous colonial correspondence vs. modern judicial interpretations. The Supreme Court''s ''honour of the Crown'' doctrine (Haida 2004) treats the reading as legally operative regardless of original intent.',
    'If reconstruction, the reading''s claimed_type (tangled_rope) may overstate the coordination function historically present; if original understanding, the high historical extractiveness metrics measure state breach of the actual treaty bargain, not the treaty itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the stewardship framing is original treaty meaning or modern legal construction.').

omega_variable(
    extraction_measurement_in_coordination_frame,
    'How to quantify extraction in a constraint that the reading defines as mutual coordination — when the reading itself denies the extraction frame?',
    'Measure the delta between treaty-promised joint management and actual unilateral state decision-making; measure revenue flows from territorial resources to Indigenous vs. state/corporate recipients; measure consent vetoes exercised vs. overridden.',
    'If extraction is measured against the reading''s own coordination standard, ε is high (breach of mutual obligation). If measured against state law standard, ε is lower (state fulfills statutory duties). The ε-invariance principle requires the reading''s own referent — so extraction is the gap between stewardship promise and unilateral practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_measurement_in_coordination_frame, empirical, 'Measurement referent problem: extraction relative to which baseline?').

omega_variable(
    mutual_obligation_asymmetry,
    'Are the ''mutual obligations'' of the stewardship reading structurally symmetric, or does the settler state''s obligation (consent) fundamentally differ from Indigenous nations'' obligation (sharing territory they already own)?',
    'Analyze whether Indigenous nations ceded any rights they did not already hold (they did not — they shared access to territory they governed). The settler state received territory and legitimacy; Indigenous nations received promises of protection and sharing. The obligation asymmetry is structural: one party shares what is theirs; the other promises not to take more.',
    'If obligations are structurally asymmetric, the claimed tangled_rope (coordination + extraction) may understate the extraction — the coordination frame itself may be a cover for the original taking. This would push classification toward snare for the Indigenous seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_obligation_asymmetry, conceptual, 'Whether mutual obligation language masks a structural asymmetry in what each party gives up.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (Indian Act, residential schools, pass system, criminalization of Indigenous governance) or internalized (Indigenous nations administering state-designed consultation processes, accepting revenue-sharing as substitute for consent)?',
    'Post-exit suppression trajectory: where Indigenous nations achieve modern treaties or self-government agreements, does suppression persist in new forms (fiscal dependency, federal oversight, defined jurisdiction boxes)? If yes, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint reproduces itself through Indigenous institutions shaped by colonial law. This would increase χ for the Indigenous seat beyond what structural metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in treaty implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 260).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__stewardship_reading, theater_ratio, 150, 0.42).
narrative_ontology:measurement(hist_tr_t200, historical_treaty_substrate__stewardship_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(hist_tr_t260, historical_treaty_substrate__stewardship_reading, theater_ratio, 260, 0.41).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__stewardship_reading, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(hist_be_t200, historical_treaty_substrate__stewardship_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(hist_be_t260, historical_treaty_substrate__stewardship_reading, base_extractiveness, 260, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__stewardship_reading, suppression_requirement, 150, 0.52).
narrative_ontology:measurement(hist_su_t200, historical_treaty_substrate__stewardship_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(hist_su_t260, historical_treaty_substrate__stewardship_reading, suppression_requirement, 260, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.08).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, modern_treaty_implementation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, duty_to_consult_jurisprudence).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, resource_revenue_sharing_regimes).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, undrip_implementation_act).

% DUAL FORMULATION NOTE:
% This constraint is the stewardship_reading of the historical_treaty_substrate kernel. It differs from extinguishment_reading (ε near 0 for state, high for Indigenous — claims cession complete) and nation_to_nation_reading (ε moderate, frames treaty as international law instrument). The ε values differ because the referent arrangement is assessed differently: stewardship sees ongoing extraction from breach of mutual obligation; extinguishment sees settled property rights; nation_to_nation sees evolving international obligations. All three linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, organized, 0.35).
constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
