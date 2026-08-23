% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Treaty Extinguishment Doctrine — Completed Property Transaction Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   The extinguishment reading treats historical treaties between the Crown
 *   and Indigenous nations as completed property transactions: Indigenous
 *   parties ceded territorial sovereignty in exchange for defined reserves
 *   and annuity payments. This reading operates as a binding legal constraint
 *   in Canadian, Australian, New Zealand, and U.S. jurisprudence — it is the
 *   doctrine that 'the treaty settled it.' The reading claims the arrangement
 *   is a rope (fair exchange, mutual benefit, coordination complete). The
 *   authored metrics describe a constraint that extracts sovereignty through
 *   active suppression, maintains itself by foreclosing alternatives, and
 *   meets sustained resistance — the engine will compute the divergence. The
 *   structural delta for this reading: Indigenous nations are coded as
 *   beneficiaries for narrow treaty rights (reserves, annuities) but payers
 *   for the territorial jurisdiction extracted; the settler state is the
 *   agenda-setter and primary beneficiary of the extracted sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.85).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.9).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaty Extinguishment Doctrine — Completed Property Transaction Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '5a180379-bfe8-4192-acf6-afa645585bda').
narrative_ontology:cs_kernel_codification('5a180379-bfe8-4192-acf6-afa645585bda', fixed_text).
narrative_ontology:cs_authority_grounding('5a180379-bfe8-4192-acf6-afa645585bda', extraction).
narrative_ontology:cs_interpretation_layer_present('5a180379-bfe8-4192-acf6-afa645585bda').
narrative_ontology:cs_reading_relation('5a180379-bfe8-4192-acf6-afa645585bda', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('5a180379-bfe8-4192-acf6-afa645585bda', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('5a180379-bfe8-4192-acf6-afa645585bda', foundational, treaty_cession_extinguishes_sovereignty).
narrative_ontology:cs_axiom_status(treaty_cession_extinguishes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5a180379-bfe8-4192-acf6-afa645585bda', treaty_cession_extinguishes_sovereignty, conventional).
narrative_ontology:cs_axiom('5a180379-bfe8-4192-acf6-afa645585bda', secondary, reserve_annuity_fulfills_treaty_obligation).
narrative_ontology:cs_axiom_status(reserve_annuity_fulfills_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5a180379-bfe8-4192-acf6-afa645585bda', reserve_annuity_fulfills_treaty_obligation, conventional).
narrative_ontology:cs_reference_frame('5a180379-bfe8-4192-acf6-afa645585bda', imperial_sovereignty_framework).
narrative_ontology:cs_drift_state('5a180379-bfe8-4192-acf6-afa645585bda', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a180379-bfe8-4192-acf6-afa645585bda', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_population).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_interests).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, crown_sovereignty_derives_from_treaty_cession).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, property_law_governs_indigenous_land_relations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, treaty_obligations_are_exhausted_by_reserve_annuity_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty regime through legislation, courts, and bureaucratic apparatus. Claims clear title to ceded territories as the legal foundation of state sovereignty. Enforces extinguishment doctrine to legitimize resource allocation, infrastructure development, and jurisdictional authority. Collects the primary benefit: undisputed territorial sovereignty.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Parties to historical treaties who, under the extinguishment reading, ceded territorial sovereignty in exchange for reserve lands and annuity payments. The reading treats this as a completed transaction: sovereignty was the price paid, reserves and annuities the consideration received. In practice, reserve lands are fraction of traditional territories, annuities are trivial and often unpaid, and the 'completed transaction' framing forecloses claims to broader jurisdiction or revenue sharing. Exit from this identity is structurally blocked — nationhood is constituted through the treaty relationship itself.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_nations, beneficiary).

% Hold fee-simple title to land derived from the Crown's asserted sovereignty over ceded territories. Benefit from secure property rights, stable governance, and resource access that the extinguishment doctrine underwrites. Their tenure depends on the doctrine's validity; challenging it would destabilize the property system they inhabit.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_population, beneficiary,
    organized, biographical, mobile, national).

% Corporate actors (mining, forestry, energy, hydro) who acquire resource rights from the Crown on ceded territories. The extinguishment doctrine provides the legal certainty that their titles are unencumbered by Indigenous jurisdiction. They lobby to maintain the doctrine and oppose interpretations that would require consent or revenue sharing.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_interests, beneficiary,
    powerful, biographical, arbitrage, national).

% UN treaty bodies, international courts, and human rights mechanisms that assess the extinguishment doctrine against standards of free, prior, and informed consent; self-determination; and the prohibition on unilateral extinguishment of Indigenous rights. They document the gap between the doctrine's claim of fair exchange and the structural asymmetry of the treaty process.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% Indigenous litigants and legal teams advancing stewardship or nation-to-nation readings in domestic courts. They are structurally excluded from the extinguishment reading's internal logic — the doctrine treats their arguments as foreclosed by the completed transaction. Their exclusion is enforced through procedural bars (limitations, estoppel) and substantive doctrines (extinguishment, sovereignty).
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_rights_litigants, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal foundation for land title, resource allocation, and state jurisdiction across the settled territory by treating historical treaties as final settlements that extinguish prior Indigenous sovereignty and vest full authority in the Crown.
% TRANSFER_FUNCTION: Moves territorial sovereignty and resource jurisdiction from Indigenous nations to the settler state, in exchange for reserve land set-asides and annuity payments. The transfer is framed as a one-time, completed transaction rather than an ongoing relationship.
% ABSENT_VOICES: Indigenous nations at the time of treaty-making — who negotiated in their own languages, under their own legal orders, and with no conception of feudal cession — are absent from the extinguishment reading's reconstruction of intent. Their understanding of the agreements as sharing or rental arrangements, not sales, is excluded by the doctrine's interpretive framework.
% DISAPPEARANCE_RATIONALE: If the extinguishment doctrine vanished overnight, the legal basis for Crown title over vast territories would collapse. Resource tenures, municipal governments, infrastructure rights, and private property chains would face existential uncertainty. Indigenous nations would assert jurisdiction over traditional territories. The state would need to negotiate new consensual arrangements — a fundamental rearrangement of the constitutional order.
% FOUNDING_PROBLEM: The Crown needed a legal mechanism to assert sovereignty over Indigenous territories and enable settler colonization without perpetual warfare or prohibitively expensive conquest. The treaty process, reinterpreted as cession, provided a lawful veneer for what was often coerced or misunderstood displacement.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — securing Crown sovereignty for colonization — is acknowledged as the historical motive by scholars outside the benefiting parties (e.g., J.R. Miller, 'Compact, Contract, Covenant'; John Borrows, 'Canada's Indigenous Constitution'; Royal Commission on Aboriginal Peoples). The state's own Royal Proclamation of 1763 and treaty commissioners' private correspondence corroborate that the Crown sought surrender of title, not mutual stewardship. No credible source outside the Crown's legal apparatus maintains the founding problem (peaceful colonization via fair purchase) is still live.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint transfers the vast majority of land value and jurisdictional authority from Indigenous nations to the settler state, with reserves constituting a small fraction of traditional territories and annuities being nominal and often unpaid. Suppression is very high (0.9) because the doctrine's persistence depends on active legal enforcement: courts strike down Indigenous jurisdiction claims, legislation extinguishes rights unilaterally, and the property system would destabilize if the doctrine were abandoned. Theater ratio is moderate (0.3): the 'honour of the Crown' rhetoric and duty to consult perform a conciliatory function, but the underlying extinguishment logic remains the default. Accessibility collapse is high (0.82) — alternatives like nation-to-nation or shared jurisdiction are legally foreclosed by the doctrine itself. Resistance is substantial (0.7) — Indigenous nations have never accepted extinguishment, pursuing litigation, political action, and international advocacy continuously.
 *
 * PERSPECTIVAL GAP:
 *   From the settler_state seat, the constraint appears as a rope: a historical deal that solved the coordination problem of peaceful settlement. From the indigenous_nations seat, the same constraint operates as a snare: the 'transaction' was coercive, the consideration was grossly inadequate, and the enforcement machinery prevents exit. The engine computes this divergence from the declared power/exit/role structure — the claimed_type ('rope') does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler_state is the agenda-setter and primary beneficiary (d ≈ 0.1): it wrote the rules, enforces them, and collects the sovereignty rent. Settler_population and resource_interests are beneficiaries with mobile/arbitrage exit (d ≈ 0.2–0.3): they benefit from the property system but could relocate capital or personhood. Indigenous_nations are payers with identity_locked exit (d ≈ 0.9): they bear the cost of sovereignty loss, their nationhood is constituted through the treaty relationship (exit = dissolution), and the reading itself codes them as beneficiaries for reserves — a structural double-bind. International_observers are analytical (d = 0.5). Indigenous_rights_litigants are excluded — their structural position is that the constraint denies them standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Crown sovereignty for colonization) is dead — colonization is complete, the Crown's sovereignty is established by other means (effective control, international recognition, constitutional patriation). Yet the constraint persists and expands (modern treaty extinguishment clauses, comprehensive claims policies requiring 'certainty' via extinguishment). The mandate has atrophied into a rent-extraction mechanism: the doctrine now functions to maintain resource access and jurisdictional monopoly, not to solve the original coordination problem. This is mandatrophy — the constraint survives because the cost of fixing it (renegotiating the constitutional order) is prohibitive for the state, and the beneficiaries (settlers, resource companies) capture the gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the extinguishment_reading''s structural classification change if the kernel (historical_treaty_substrate) is read through the stewardship_reading or nation_to_nation_reading instead?',
    'Generate sibling constraint stories for stewardship_reading and nation_to_nation_reading with their own beneficiary/victim structures, metrics, and claimed_types. Compare the engine''s per-seat classifications across the three readings.',
    'If sibling readings produce substantially different classifications (e.g., stewardship_reading computes as mountain or rope for Indigenous seats), the kernel itself is the site of contestation — no single reading captures the constraint''s structure. The extinguishment_reading''s claimed_type=''rope'' would be exposed as a reading-specific claim, not a property of the treaty relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment-system framing under-determination: the extinguishment_reading is one of three coherent framings of the same kernel, each producing different structural classifications.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.9) primarily structural (courts, police, legislation) or partially internalized (Indigenous nations accepting the extinguishment narrative as foreclosing their own claims)?',
    'Post-litigation trajectory analysis: when courts reject extinguishment arguments (e.g., Tsilhqot''in, Haida), does Indigenous political action escalate (suggesting suppression was structural and removable) or remain constrained (suggesting internalized suppression persists)?',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure — the target carries the foreclosure with them even after legal barriers are lowered. This would increase effective extraction for identity_locked agents beyond the engine''s current computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the treaty extinguishment context.').

omega_variable(
    transaction_framing_as_cover,
    'Does the ''completed property transaction'' framing function as a cover story that masks ongoing extraction, or does it accurately describe a historical exchange that exhausted the obligations?',
    'Comparative analysis of treaty texts in Indigenous languages, oral histories, and negotiation records versus the Crown''s written English/French versions. If the parties'' understandings were fundamentally incompatible (cession vs. sharing), the transaction framing is a post-hoc imposition.',
    'If the framing is a cover, the constraint is a snare masquerading as a rope — the coordination function (peaceful settlement) is real but the extraction (sovereignty for trivial consideration) is the dominant structural feature. The claimed_type=''rope'' would be a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transaction_framing_as_cover, conceptual, 'Whether the extinguishment reading''s core premise (fair exchange) is a genuine coordination narrative or an extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1763, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_ext_tr_t1763, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1763, 0.15).
narrative_ontology:measurement(hts_ext_tr_t1813, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1813, 0.2).
narrative_ontology:measurement(hts_ext_tr_t1867, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1867, 0.25).
narrative_ontology:measurement(hts_ext_tr_t1927, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1927, 0.35).
narrative_ontology:measurement(hts_ext_tr_t1973, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(hts_ext_tr_t1990, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(hts_ext_tr_t2024, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(hts_ext_be_t1763, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1763, 0.65).
narrative_ontology:measurement(hts_ext_be_t1813, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1813, 0.72).
narrative_ontology:measurement(hts_ext_be_t1867, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1867, 0.78).
narrative_ontology:measurement(hts_ext_be_t1927, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1927, 0.83).
narrative_ontology:measurement(hts_ext_be_t1973, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1973, 0.85).
narrative_ontology:measurement(hts_ext_be_t1990, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1990, 0.84).
narrative_ontology:measurement(hts_ext_be_t2024, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hts_ext_su_t1763, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1763, 0.7).
narrative_ontology:measurement(hts_ext_su_t1813, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1813, 0.8).
narrative_ontology:measurement(hts_ext_su_t1867, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1867, 0.88).
narrative_ontology:measurement(hts_ext_su_t1927, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1927, 0.92).
narrative_ontology:measurement(hts_ext_su_t1973, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1973, 0.9).
narrative_ontology:measurement(hts_ext_su_t1990, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(hts_ext_su_t2024, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__extinguishment_reading, 0.15).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indian_act_governance).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, comprehensive_claims_policy).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, duty_to_consult_jurisprudence).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_revenue_sharing_regimes).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indigenous_self_government_agreements).

% DUAL FORMULATION NOTE:
% This constraint (extinguishment_reading) is one of three readings of the historical_treaty_substrate kernel. The stewardship_reading and nation_to_nation_reading are sibling constraints with different beneficiary/victim structures and claimed_types. All three form a constraint family linked by network.affects_constraints. The extinguishment_reading's ε (0.85) is substantially higher than the stewardship_reading's expected ε (~0.15) because the latter treats the treaty as a living coordination mechanism, not an extraction event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
