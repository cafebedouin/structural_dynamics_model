% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Electronic Money Emergence via Conceptual Thinkability
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel:
 *   electronic money's emergence. The reading claims that digital money
 *   emerged when the conceptual possibility became technically and socially
 *   thinkable—when researchers, cryptographers, and engineers shared a
 *   working vision that money could be dematerialized and computed, prior to
 *   any institutional measurement or adoption. This reading locates emergence
 *   in the cognitive and theoretical domain (1960s–1980s), not in
 *   institutional fact (which came later). The constraint describes the
 *   social structure around this thinkability: who benefited from it, who
 *   bore its costs (institutional uncertainty), what coordination problems it
 *   solved (aligning scattered technical researchers into a shared vision),
 *   and what transfer it effected (moving intellectual authority from
 *   monetary economists to computer scientists). The reading is not claimed
 *   as true or false; it is one structurally coherent reading of the
 *   contested kernel, and the schema enforces only that it be internally
 *   consistent, ε-invariant, and clearly distinguished from its siblings.
 *
 * KEY AGENTS:
 *   - Early computing researchers (MIT, Bell Labs): developed conceptual frames treating money as information, enabling the thought-experiment
 *   - Cryptographic theorists (Diffie, Hellman, Chaum): supplied the mathematical apparatus that made digital-money protocols thinkable
 *   - Financial technologists: implemented proof-of-concept systems (DigiCash, e-gold), realizing the thinkability into working code
 *   - Central banks and commercial banks: bore the cost of institutional uncertainty and measurement dislocation as thinkability emerged
 *   - Regulatory observers: tracked the emergence, responded with policy frameworks after the fact
 *   - Traditional monetary economists: largely excluded from the conceptual innovation, forced to respond reactively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.31).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Electronic Money Emergence via Conceptual Thinkability").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'bf165a13-1a76-4e5f-8452-946a1f496afe').
narrative_ontology:cs_kernel_codification('bf165a13-1a76-4e5f-8452-946a1f496afe', distributed).
narrative_ontology:cs_authority_grounding('bf165a13-1a76-4e5f-8452-946a1f496afe', expertise).
narrative_ontology:cs_interpretation_layer_present('bf165a13-1a76-4e5f-8452-946a1f496afe').
narrative_ontology:cs_reading_relation('bf165a13-1a76-4e5f-8452-946a1f496afe', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_reading_relation('bf165a13-1a76-4e5f-8452-946a1f496afe', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('bf165a13-1a76-4e5f-8452-946a1f496afe', foundational, emergence_is_cognitive_event).
narrative_ontology:cs_axiom_status(emergence_is_cognitive_event, holdable).
narrative_ontology:cs_axiom_grounding('bf165a13-1a76-4e5f-8452-946a1f496afe', emergence_is_cognitive_event, deontological).
narrative_ontology:cs_axiom('bf165a13-1a76-4e5f-8452-946a1f496afe', secondary, thinkability_precedes_measurement).
narrative_ontology:cs_axiom_status(thinkability_precedes_measurement, holdable).
narrative_ontology:cs_axiom_grounding('bf165a13-1a76-4e5f-8452-946a1f496afe', thinkability_precedes_measurement, empirically_contingent).
narrative_ontology:cs_reference_frame('bf165a13-1a76-4e5f-8452-946a1f496afe', monetary_theory_physicalist_classical).
narrative_ontology:cs_drift_state('bf165a13-1a76-4e5f-8452-946a1f496afe', post_cryptographic_revolution, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('bf165a13-1a76-4e5f-8452-946a1f496afe', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, early_computing_researchers).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, cryptographic_theorists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, financial_technologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, users_general_population).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, central_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and industry researchers (MIT, Bell Labs, early computer science departments) who developed the conceptual frameworks enabling digital currency thought. They framed money as computable information, separable from physical substrate. Their intellectual contributions shaped the horizon of what became technically thinkable; they collected prestige and research funding aligned with exploring computation's economic applications.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_computing_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Mathematicians and computer scientists (Diffie, Hellman, later Chaum) who supplied the theoretical apparatus (public-key cryptography, zero-knowledge proofs, commitment schemes) that made digital-money schemes mathematically conceivable. Their work bridged abstract mathematics to economic protocol design. They benefited through academic careers, patent positions, and intellectual authority in a new domain.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, cryptographic_theorists, beneficiary,
    powerful, generational, arbitrage, global).

% Engineers and entrepreneurs who implemented proof-of-concept systems (DigiCash, e-gold, early cryptocurrency experiments). They realized the thinkability into working code, collecting entrepreneurial prestige, venture capital, and first-mover positioning in an emerging market segment.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, financial_technologists, beneficiary,
    organized, biographical, mobile, global).

% Monetary authorities initially bore the institutional uncertainty of digital money's thinkability — they faced new theoretical challenges to their monopoly control over currency issuance, required new regulatory frameworks, and had to either integrate or suppress the emerging technologies. Their surveillance and measurement capacity was dislocated by the emergence of forms of value that initially escaped institutional accounting.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_banks, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, central_banks, observer).

% Faced the prospect of disintermediation once digital money became thinkable as a peer-to-peer technology. They bore the cost of legacy system updates to accommodate the new conceptual frame, and later the strategic uncertainty of whether their role would be preserved or displaced by direct digital-currency rails.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, commercial_banks, payer,
    institutional, generational, constrained, national).

% Eventual users of digital money (when it became institutionalized). During the emergence phase, they existed only as potential beneficiaries of the technology once thinkability crystallized into deployment. They had no agency in the conceptual evolution; they inherited the frame once it became socially established.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, users_general_population, beneficiary,
    powerless, biographical, mobile, global).

% Government agencies, legislatures, and policy analysts who tracked the emergence of digital-money concepts and debated whether to permit, regulate, or suppress them. They occupied an analytical seat, observing the conceptual shift and choosing institutional response after the fact.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, regulatory_observers, observer,
    institutional, generational, analytical, national).

% Economists and theorists whose frameworks treated money as necessarily physical or necessarily state-backed. They had little voice in the emergence phase because the conceptual innovation specifically bypassed their frameworks. Their resistance was a form of exclusion from the communities where electronic-money thinkability developed.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, traditional_money_conceptualists, excluded,
    organized, generational, constrained, national).

% Statistical agencies (Federal Reserve, national central banks) that maintained monetary aggregates (M1, M2, M3, M4, M5). They faced a fundamental challenge: the emergence of digital money as thinkable called their measurement categories into question, eventually forcing the reconceptualization of M4/M5 definitions in the 1990s–2000s.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, measurement_apparatus_keepers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared conceptual frame within the research and engineering community: the possibility that money can be dematerialized, computed, and transmitted as information rather than physical objects. The coordination problem solved is aligning technical researchers, cryptographers, and financial engineers on a common vision of what becomes computationally and economically feasible.
% TRANSFER_FUNCTION: Moves intellectual authority and prestige from traditional monetary theory toward computational and cryptographic frameworks. Transfers research funding, career opportunities, and institutional positioning from conventional banking and central-bank economists toward computer scientists and mathematicians. Moves social imagination: the population's conception of what money *is* shifts from a scarce physical commodity toward a dematerialized information state.
% ABSENT_VOICES: Traditional monetary economists and central bankers were largely excluded from the emergence phase. They had not accepted the conceptual premises and so did not participate in early digital-money protocol development. Later, they were forced to respond reactively to thinkability that had already crystallized in other communities.
% DISAPPEARANCE_RATIONALE: If the emergence of digital-money-as-thinkable had never occurred (counterfactually: if computing remained unable to be applied to monetary theory), the world's monetary institutions would have developed differently. The conceptual possibility constrained and enabled all subsequent institution-building around cryptocurrency, central-bank digital currencies, and payment-system modernization. Without the thinkability, those technologies would not have emerged as inevitable or natural solutions to financial problems.
% FOUNDING_PROBLEM: Money's substrate was unnecessarily bound to physical objects and state-backed institutions. The computational revolution offered a proof-of-concept that information systems could solve the double-spending problem, settlement timing, and value transfer without physical carriers or centralized custodians. The problem was: can computation replace physicality in the fundamental function of money?
% FOUNDING_PROBLEM_CORROBORATION: Computer scientists and cryptographers attest that the founding problem was live and that their work solved it theoretically. Central banks and regulatory authorities now attest, in retrospect, that the problem was real (they adopted CBDC research agendas based on this framing). However, traditional monetary economists dispute whether the problem was real or was instead an artificial reframing of an already-solved coordination mechanism. The absence of corroboration from outside the benefiting technical community during the emergence phase is the signal itself: the thinkability emerged within a closed intellectual circle and was imposed on monetary institutions retroactively.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the constraint describes an intellectual and social process, not direct economic transfer. The extraction is in reallocation of prestige, research funding, and intellectual authority away from traditional monetary frameworks toward computational ones. Suppression is modest (0.31) because the emergence was not primarily enforced top-down; it was a bottom-up intellectual shift within academic and technical communities. However, suppression increased over time (1960–2000) as central banks and commercial institutions attempted to contain or regulate the emerging thinkability. Theater ratio is low (0.22) because the work was genuinely technical and theoretical; performative elements were minimal during the emergence phase. Resistance is moderate-to-high (0.55) because traditional monetary authorities and economists actively resisted the reframing. The measurement series show a gradual diffusion trajectory: thinkability accumulates, institutional resistance hardens, and by 2000 the conceptual frame has become widely established (setting conditions for the cryptocurrency and CBDC research agendas of the 2010s–2020s). All metrics are authored at shared time points (1960, 1970, 1980, 1990, 2000) to enable lifecycle drift detection. The early measurements (1960–1970) are projected based on the reading's premise that thinkability preceded measurement; later measurements are observed from published research volumes, patent filings, and institutional responses.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (researchers, cryptographers, technologists) experience this constraint as enabling and emancipatory—it opened a new research frontier and created intellectual opportunity. The payer seats (central banks, commercial banks) experience it as threatening and dislocating—it undermined their monopoly on monetary authority and forced them into reactive institutional adaptation. The gap is structural: one party's frame-creation is another party's frame-destruction. The engine computes this divergence from the directionality logic: beneficiaries have low d (they accrue intellectual authority), payers have higher d (they bear institutional uncertainty and threat to established hierarchies).
 *
 * DIRECTIONALITY LOGIC:
 *   Early computing researchers and cryptographic theorists are the structural beneficiaries (d near 0.2): they collected intellectual authority, research funding, career opportunities, and the ability to reshape what counts as a legitimate approach to monetary problems. They had high exit options (arbitrage: could leave monetary-theory circles for pure mathematics or computer science) and faced no institutional suppression. Financial technologists are secondary beneficiaries (d near 0.3): they realized thinkability into code and products, collecting entrepreneurial prestige and venture capital, but faced growing regulatory suppression as central banks became aware of the threat. Central banks and commercial banks are the payers (d near 0.75): they bore the cost of institutional uncertainty, loss of monopoly legitimacy, requirement to update measurement frameworks, and threat to their gatekeeping role in monetary systems. Their exit options were constrained (identity_locked: 'we are the monetary authority' is their institutional identity) and they faced pressure to either adopt or suppress the emerging thinkability. Users (general population) are beneficiaries in the long arc but had zero agency during emergence (d near 0.0: they were passive recipients of a conceptual shift they did not participate in shaping).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (can computation replace physicality in money's substrate?) was live during emergence (1960–2000) among technical researchers. By 2020, after cryptocurrency maturation and central-bank CBDC programs, the problem is contested: some authorities (regulators, digital-currency advocates) treat it as solved in principle, while others (traditional economists, retail banking advocates) dispute whether it was ever a real problem. The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges suggests the arrangement (the thinkability structure) will persist not because the founding problem remains live, but because institutional lock-in and path dependence have made the conceptual frame irreversible. This is a candidate for future mandatrophy if the founding problem becomes unambiguously dead (i.e., if computation proves insufficient for monetary functions and institutions revert to physicalist frameworks).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_feasibility_boundary,
    'When does a conceptual possibility become ''thinkable'' versus merely theoretically interesting? Is there a bright-line distinction between academic speculation (digital money could work) and genuine social thinkability (digital money is a real option for organizing society)?',
    'Historical analysis of adoption patterns: if the emergence of institutions using digital money follows naturally from the thinkability without significant gap, thinkability is real; if adoption is delayed decades after thinkability is theoretically established, the gap itself is evidence that additional social conditions were required.',
    'If the boundary is sharp and empirically locatable, the reading''s emergence date can be refined. If thinkability is merely a gradient of increasing consensus, the reading''s precise emergence moment becomes indeterminate, suggesting the sibling readings (first_held, m4_m5_collapse) capture more observable emergence events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_feasibility_boundary, conceptual, 'The ontological status of thinkability as a locatable historical event versus a retrospective narrative construction.').

omega_variable(
    measurement_lag_or_emergence_lag,
    'Institutional measurement of electronic money (M4/M5 redefinition) lagged conceptual emergence by decades. Is this lag evidence that emergence truly occurred before measurement, or is measurement lag simply a bureaucratic delay that tells us nothing about when emergence actually occurred?',
    'Comparative analysis of other technologies where thinkability, first use, and institutional measurement are decoupled. Does early thinkability in telephony, aviation, or computing correlate predictably with institutional measurement lag? Or is the pattern idiosyncratic to monetary systems?',
    'If the lag is a general feature of how institutions respond to technical innovation, the reading''s claim is supported. If the lag is specific to monetary systems'' resistance to category change, measurement lag becomes an artifact of institutional suppression, not evidence of earlier emergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_lag_or_emergence_lag, empirical, 'Whether measurement lag in monetary statistics evidences actual emergence timing or merely institutional inertia.').

omega_variable(
    community_vs_general_thinkability,
    'Thinkability was achieved within specialized technical communities (computer scientists, cryptographers, some financial engineers) decades before the general public or mainstream monetary institutions accepted it. Does emergence count from community thinkability (the reading''s position) or only from general-population or institutional thinkability?',
    'Definition choice: if emergence is defined as ''a specialized community can coherently imagine it,'' the reading''s 1960s–1980s dating holds. If emergence requires ''the general social order accepts it as a live option,'' the reading must push the date forward to the cryptocurrency boom (2010s) or CBDC institutionalization (2020s). The choice depends on whether emergence is a cognitive or social fact.',
    'The reading would shift from pure intellectual emergence (1960s) to bifurcated emergence (intellectual by 1970s, social by 2010s) if general-population thinkability is the gate. This would align the reading more closely with the first_held_reading (which locates institutional emergence at 1980s–1990s).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_vs_general_thinkability, preference, 'Whether emergence is located in specialized-community thinkability or population-wide social acceptance.').

omega_variable(
    suppression_as_affordance,
    'The constraint''s suppression increased over time (central banks attempted to regulate or contain digital-money concepts). Was this suppression a sign that thinkability was real (institutions felt threatened enough to react), or evidence that thinkability had not yet crystallized (institutions would not suppress a merely academic curiosity)?',
    'Comparative suppression analysis: institutions typically suppress emerging threats only when they believe the threat is credible and imminent. Rising suppression (1970–2000) is consistent with genuine thinkability that threatened institutional interests.',
    'If rising suppression is the signal, the reading''s claim is strengthened: central banks'' own behavior validates that thinkability had become institutionally salient. If suppression is reframed as institutional overreaction to a merely theoretical threat, thinkability remains speculative until institutional deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_affordance, empirical, 'Whether rising institutional suppression evidences the reality of emerged thinkability or merely institutional anxiety about academic speculation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.22).

% Extraction over time
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1990, 0.29).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2000, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% Electronic money emergence is contested across three structurally distinct readings of a single kernel: (1) became_thinkable_reading (this constraint): emergence in conceptual/intellectual domain, 1960s–1980s; (2) first_held_reading: emergence in institutional adoption, 1980s–1990s; (3) m4_m5_collapse_reading: emergence retroactively created by statistical measurement change, 1990s–2000s. Each reading has a different ε (thinkability is low-extraction coordination; institutional adoption is higher-extraction because institutions resist; measurement artifacts are deceptive, high-extraction if measurement creates false categories). The three stories are linked: thinkability must precede institutional adoption, and institutional adoption drives measurement redefinition. Consumption order: became_thinkable_reading → first_held_reading → m4_m5_collapse_reading captures causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
