% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Constitution — Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic's dual executive (president + prime minister) was
 *   designed for aligned majorities. When legislative elections produce a
 *   majority opposing the president (cohabitation, occurring 1986-88,
 *   1993-95, 1997-2002, 2022-24), the constitution's ambiguity becomes
 *   operational: president retains reserved domains (foreign, defense,
 *   nuclear) while PM controls domestic policy and legislation. This reading
 *   treats cohabitation as the equilibrium state where both executives
 *   constrain each other through negotiated authority allocation. Extraction
 *   is moderate (0.42 at interval end) but unstable — peaking during
 *   cohabitation when negotiation friction is highest, falling during
 *   alignment when president dominates. The beneficiary alternates by domain;
 *   the victim is policy coherence and citizen legibility. The constraint
 *   requires active enforcement (Constitutional Council arbitration,
 *   political negotiation rituals). Classified as tangled_rope: genuine
 *   coordination (dual legitimacy resolved) with asymmetric extraction
 *   (domain-split benefits, coherence costs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.38).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution — Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '494b5ee1-0c60-482d-9a54-14486d6f3cae').
narrative_ontology:cs_kernel_codification('494b5ee1-0c60-482d-9a54-14486d6f3cae', fixed_text).
narrative_ontology:cs_authority_grounding('494b5ee1-0c60-482d-9a54-14486d6f3cae', lineage).
narrative_ontology:cs_interpretation_layer_present('494b5ee1-0c60-482d-9a54-14486d6f3cae').
narrative_ontology:cs_reading_relation('494b5ee1-0c60-482d-9a54-14486d6f3cae', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('494b5ee1-0c60-482d-9a54-14486d6f3cae', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('494b5ee1-0c60-482d-9a54-14486d6f3cae', foundational, dual_legitimacy_requires_negotiation).
narrative_ontology:cs_axiom_status(dual_legitimacy_requires_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('494b5ee1-0c60-482d-9a54-14486d6f3cae', dual_legitimacy_requires_negotiation, conventional).
narrative_ontology:cs_axiom('494b5ee1-0c60-482d-9a54-14486d6f3cae', foundational, domain_split_foreign_domestic).
narrative_ontology:cs_axiom_status(domain_split_foreign_domestic, holdable).
narrative_ontology:cs_axiom_grounding('494b5ee1-0c60-482d-9a54-14486d6f3cae', domain_split_foreign_domestic, conventional).
narrative_ontology:cs_reference_frame('494b5ee1-0c60-482d-9a54-14486d6f3cae', aligned_majority_presidentialism).
narrative_ontology:cs_drift_state('494b5ee1-0c60-482d-9a54-14486d6f3cae', recurrent_cohabitation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('494b5ee1-0c60-482d-9a54-14486d6f3cae', '2026-08-14T14:22:10Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_foreign_policy).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_domestic_policy).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, citizen_legibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, dual_executive_legitimacy).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_ambiguity_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds reserved domains (foreign policy, defense, nuclear) and appoints PM but cannot govern without legislative support. During cohabitation, must negotiate with Assembly-backed PM; leverages symbolic authority and reserved powers to shape agenda. Exit constrained by fixed term and constitutional position.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary).

% Controls parliamentary majority and domestic policy implementation. Must negotiate with president on reserved domains and decree authority. Accountable to Assembly; can be dismissed only through legislative defeat. Exit constrained by parliamentary responsibility.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% Elects PM and controls legislative agenda. Gains policy implementation capacity during cohabitation but must manage dual-executive friction. Can dissolve government via censure but cannot directly remove president. Exit mobile through elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    organized, biographical, mobile, national).

% Excluded from executive power during cohabitation; can only influence through parliamentary procedure and public pressure. Structural position incentivizes constitutional crisis narratives to force realignment. Exit constrained by electoral calendar.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_opposition, excluded,
    organized, biographical, constrained, national).

% The abstract good of consistent, legible government action across domains. Bears the cost of dual-executive negotiation: delayed decisions, contradictory signals, implementation gaps between foreign and domestic policy. No exit — is the construct being measured.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% The public's ability to attribute responsibility and hold power accountable. Blurred when two executives claim overlapping mandates. Suffers during cohabitation from conflicting narratives about who governs. No exit — is the construct being measured.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, citizen_legibility, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, citizen_legibility).

% Adjudicates boundary disputes between president and PM when referred. Jurisprudence has expanded review scope but avoids political questions. Neither collects nor pays; provides the interpretive layer that stabilizes the equilibrium.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Analyze the French model as a case of semi-presidentialism. Produce the taxonomic vocabulary (cohabitation, dual legitimacy) that shapes how practitioners understand their own constraint. No stake in French outcomes.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, comparative_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of executive legitimacy in a system with two popular mandates (president elected nationally, PM backed by parliamentary majority) by allocating authority along a domain split: reserved domains to president, legislative/domestic to PM, with negotiation required at boundaries.
% TRANSFER_FUNCTION: Moves decision-rights across the foreign-domestic boundary: president cedes domestic implementation to PM; PM cedes foreign/defense initiative to president; both transfer political capital to sustain the negotiation. The transfer is unstable — each cohabitation renegotiates the boundary.
% ABSENT_VOICES: Voters who elected both executives on competing platforms are structurally excluded from the negotiated allocation; they experience the output but not the bargaining. Small parties excluded from parliamentary majority have no seat. The constitutional text itself is silent on cohabitation — the 1958 text assumes aligned majorities.
% DISAPPEARANCE_RATIONALE: If the cohabitation equilibrium vanished (e.g., constitutional amendment fixing domains, or electoral reform eliminating divided government), executive-legislative relations would restructure: either toward hyper-presidentialism (president dominates) or parliamentary supremacy (PM dominates). The specific negotiation ritual and its policy effects would disappear.
% FOUNDING_PROBLEM: The 1958 Constitution created a dual executive to escape Fourth Republic instability but assumed presidential and parliamentary majorities would align (president's party wins Assembly). The founding problem was executive stability, not divided-government management. Cohabitation emerged in 1986 as an unanticipated configuration.
% FOUNDING_PROBLEM_CORROBORATION: De Gaulle's 1958 speeches and the constitutional debates (recorded in Journal Officiel) attest the alignment assumption. Mitterrand's 1986-88 cohabitation practice and Balladur's 1993-95 cohabitation established the equilibrium as operational. Scholars (Huber, Elgie, Schleiter) corroborate the unanticipated-emergence reading from outside French political actors.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).
:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42: moderate because the arrangement solves a real coordination problem (dual legitimacy) but extracts through domain-split negotiation friction. Suppression 0.38: moderate — alternatives (parliamentary system, presidential system) exist but require constitutional amendment; exit is constrained but not impossible. Theater 0.28: the negotiation ritual has performative elements (public letters, televised councils) but substantive bargaining occurs. Accessibility collapse 0.45: alternatives are visible but politically costly. Resistance 0.55: significant — each cohabitation generates reform proposals (term alignment, constitutional clarification) that fail. Measurement series captures three cohabitation cycles (1986, 1993, 1997, 2022) showing extraction peaks during divided government.
 *
 * PERSPECTIVAL GAP:
 *   From president's seat during cohabitation: constraint is extraction (PM encroaching on reserved domains). From PM's seat: constraint is coordination (finally able to govern). From Assembly majority: constraint is enablement. From opposition: constraint is suppression. The engine computes this divergence from the structural data. The claimed tangled_rope reflects the analytical seat seeing both coordination and extraction simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   President and PM are dual agenda_setters with secondary beneficiary roles: each controls a policy domain and extracts political capital from the negotiation. Assembly majority is beneficiary (gains implementation capacity). Opposition is excluded (structurally locked out). Policy_coherence and citizen_legibility are non-agent payers bearing diffuse costs. Constitutional Council and scholars are observers. Directionality: president and PM sit near symmetric (d ~0.5) during cohabitation — each constrains the other; Assembly majority d ~0.3 (benefits); opposition d ~0.7 (excluded); non-agent payers d ~0.6 (bear costs without voice). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (executive stability under aligned majorities) is contested — some argue it persists (presidential term still matters), others that it's dead (cohabitation proved the system works without alignment). The arrangement persists not because the founding problem is live but because constitutional amendment is hard and the equilibrium works well enough. No concentrated beneficiary captures the extraction (president and PM alternate); costs are diffuse. This is mandatrophy: the constraint's original justification has attenuated but it persists through institutional inertia and the absence of a reform coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the cohabitation_equilibrium_reading a distinct constraint from the hyper_presidential_reading and parliamentary_constraint_reading, or a contextual application of the same kernel?',
    'Test whether the three readings produce structurally different beneficiary/victim sets, extractiveness profiles, and enforcement requirements across the same historical episodes. If they do, they are distinct constraints linked by kernel_id.',
    'If distinct, each reading gets its own ε and classification; the kernel_id becomes a family link. If not, the decomposition is analytical artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate ε-invariant constraints.').

omega_variable(
    extraction_instability_source,
    'Does the measured instability in extractiveness (peaks during cohabitation, valleys during alignment) reflect genuine structural variation, or measurement artifact from shifting the referent between cohabitation and aligned periods?',
    'Hold the referent fixed (the standing arrangement of the Fifth Republic''s dual executive) and measure ε separately for cohabitation vs aligned periods. If ε differs, the constraint itself is time-varying; if ε is stable and only the active seats change, the instability is seat-composition, not constraint drift.',
    'If ε varies, the constraint has temporal structure requiring phase-specific analysis. If stable, the temporal measurements capture seat turnover, not constraint evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_instability_source, empirical, 'Source of extractiveness variation across cohabitation and aligned periods.').

omega_variable(
    beneficiary_asymmetry_mechanism,
    'Why does the beneficiary alternate between president (foreign) and PM (domestic) rather than one actor capturing both? Is the domain split structurally enforced or politically contingent?',
    'Compare Constitutional Council jurisprudence (structural enforcement) vs political practice during each cohabitation (contingent negotiation). If jurisprudence creates hard boundaries, the split is structural; if practice varies while jurisprudence is stable, the split is political.',
    'Structural split supports tangled_rope (coordination function with asymmetric extraction). Contingent split suggests the coordination function is weaker and extraction more negotiable — possibly scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_mechanism, conceptual, 'Whether the foreign-domestic beneficiary alternation is structurally fixed or politically negotiated.').

omega_variable(
    victim_ontology,
    'Are policy_coherence and citizen_legibility genuine victims (bearing costs without voice), or analytical constructs standing in for diffuse citizen harms?',
    'Test whether identifiable citizen groups experience attributable harm from cohabitation friction (e.g., delayed EU transposition, contradictory social policy signals) and whether they have organized representation. If yes, victims are real agents; if no, they are analytical placeholders.',
    'Real victims strengthen snare/tangled_rope classification. Analytical placeholders weaken victim declaration — the constraint may be rope with diffuse costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_ontology, empirical, 'Ontological status of the declared victim constructs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.22).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.31).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.38).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.45).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.32).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2017, 0.25).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.3).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.25).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.28).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2017, 0.22).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement(fifth_republic_constitution__cohabitation_equilibrium_reading_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electoral_calendar__term_alignment).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council_arbitration).

% DUAL FORMULATION NOTE:
% Fifth Republic kernel decomposes into three readings with distinct ε: hyper_presidential (ε low during alignment, beneficiary=president), parliamentary_constraint (ε low during alignment, beneficiary=Assembly), cohabitation_equilibrium (ε moderate+unstable, alternating beneficiaries). Linked as constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.55).
constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
