% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Legitimacy: Classical Literary vs. Medieval Technical
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_reading of the latin_correctness
 *   kernel. In this reading, classical norms are treated as the sole
 *   legitimate standard for literary and rhetorical Latin, while medieval
 *   forms retain legitimacy for technical and practical domains. This
 *   bifurcation creates a status hierarchy that privileges literary
 *   production and pressurizes technical practitioners toward unattainable
 *   classical standards. Sibling readings include continuity_reading
 *   (medieval Latin as organic continuation) and rupture_reading (medieval
 *   usage as pure corruption). This reading occupies the contested middle
 *   ground, generating moderate extractiveness through asymmetric prestige
 *   allocation.
 *
 * KEY AGENTS:
 *   - Humanist scholars (agenda_setter/beneficiary): Organized philological authorities who adjudicate the classical/medieval boundary and derive authority from its maintenance.
 *   - Neo-Latin literary elite (beneficiary): Powerful pan-European writers who occupy the prestige apex of the bifurcated linguistic hierarchy.
 *   - Technical practitioners (payer): Moderate-power scribes, medical writers, and legal drafters who bear the compliance costs and status penalties of the literary/technical divide.
 *   - Philological observers (observer): Analytical modern scholars who describe the constraint's sociolinguistic operation from outside its beneficiary structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.45).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.5).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Legitimacy: Classical Literary vs. Medieval Technical").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '1cefc75b-2e70-497a-95ba-689b987c8918').
narrative_ontology:cs_kernel_codification('1cefc75b-2e70-497a-95ba-689b987c8918', fixed_text).
narrative_ontology:cs_authority_grounding('1cefc75b-2e70-497a-95ba-689b987c8918', lineage).
narrative_ontology:cs_interpretation_layer_present('1cefc75b-2e70-497a-95ba-689b987c8918').
narrative_ontology:cs_reading_relation('1cefc75b-2e70-497a-95ba-689b987c8918', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cefc75b-2e70-497a-95ba-689b987c8918', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('1cefc75b-2e70-497a-95ba-689b987c8918', foundational, bifurcated_domain_legitimacy).
narrative_ontology:cs_axiom_status(bifurcated_domain_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1cefc75b-2e70-497a-95ba-689b987c8918', bifurcated_domain_legitimacy, conventional).
narrative_ontology:cs_reference_frame('1cefc75b-2e70-497a-95ba-689b987c8918', classical_literary_supremacy).
narrative_ontology:cs_drift_state('1cefc75b-2e70-497a-95ba-689b987c8918', vernacular_encroachment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1cefc75b-2e70-497a-95ba-689b987c8918', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, neo_latin_literary_elite).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate correctness in Latin usage through philological recovery of classical standards. They establish and enforce the boundary between legitimate classical literary Latin and permissible medieval technical Latin, deriving institutional authority and career capital from their role as gatekeepers of the bifurcated norm.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_scholars, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, humanist_scholars, beneficiary).

% Compose high-status literary, rhetorical, and pan-European scholarly works in classical Ciceronian or Vergilian Latin. They occupy the prestige apex of the linguistic hierarchy, collecting cultural capital from the delegitimization of medieval forms in their domain while remaining insulated from the practical pressures of technical discourse.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, neo_latin_literary_elite, beneficiary,
    powerful, generational, mobile, continental).

% Use Latin for practical, medical, legal, and technical texts where medieval syntactic and lexical forms remain functionally effective. Face upward pressure to emulate unattainable classical standards in prefaces, dedications, and formal submissions to patrons or institutions, incurring status penalties and redundant editorial labor when they fail to do so.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_practitioners, payer,
    moderate, biographical, constrained, regional).

% Study the sociolinguistic history of Latin as a descriptive phenomenon. They document the gap between the hybrid prescriptive norm and actual usage patterns, analyzing how the literary/technical bifurcation functioned as a status mechanism rather than a purely functional register distinction.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, philological_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a high-register, pan-European standard for literary and scholarly communication, preventing the complete fragmentation of Latin into mutually incomprehensible regional forms, while allowing pragmatic flexibility in technical domains where strict classical purity would impede practical communication.
% TRANSFER_FUNCTION: Moves cultural prestige, pedagogical authority, and patronage access from technical/practical domains to the literary/rhetorical sphere; transfers the labor of linguistic self-correction, educational investment, and status anxiety to technical practitioners who must navigate dual-register expectations.
% ABSENT_VOICES: Medieval Latin speakers whose organic linguistic evolution is delegitimized in the literary sphere; vernacular technical writers whose exclusion from Latin literacy is reinforced by the elevated status of classical literary Latin; monastic copyists whose pragmatic textual traditions are relegated to a lower legitimacy tier.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimacy norm vanished, technical practitioners would lose the pressure to emulate unattainable classical standards in formal contexts, the prestige premium attached to Ciceronian Latin would deflate, and humanist scholars would lose a primary source of adjudicative authority and curricular justification.
% FOUNDING_PROBLEM: The fragmentation of Latin into divergent regional and post-classical forms threatened the ideal of a unified respublica litteraria and the clarity of communication across the European scholarly community.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars attest the founding problem of medieval 'barbarism' as live. Medievalist philologists and linguistic historians outside the beneficiary set argue the problem was overstated and the hybrid arrangement functioned primarily as a status gate; their independent textual analysis and sociolinguistic research corroborate a shifted-function reading.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).
:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint extracts status and labor from technical practitioners while leaving them a partially legitimate register; suppression (0.50) reflects pedagogical and patronage enforcement rather than brute coercion. Theater ratio (0.40) captures the performative maintenance of classical purity in literary spheres where Ciceronian emulation often exceeded functional communication needs. Resistance is moderate-low (0.35) because the victim set is partial and fragmented across technical guilds. The temporal series track the rise of humanist institutionalization (extraction and enforcement intensifying through T=30) followed by slight relaxation as vernaculars encroached.
 *
 * PERSPECTIVAL GAP:
 *   The humanist and literary-elite seats experience the constraint as necessary coordination to preserve a unified European scholarly language; the technical-practitioner seat experiences the same structure as arbitrary status extraction that devalues their functional competence. The engine computes this divergence from the structural data without requiring claim reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars and the neo-Latin literary elite are structural beneficiaries: they collect prestige and authority from the classical norm (low directionality, subsidized by the constraint). Technical practitioners are the declared victims: they pay the costs of compliance and status subordination (high directionality, amplified extraction). Philological observers sit at the analytical remove with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination functionâpreserving a transnational literary standardâthe constraint would read as pure snare. Without the victim setâtechnical practitioners pressured by unattainable normsâit would read as rope or scaffold. The tangled_rope classification is warranted because the same structure that coordinates literary communication simultaneously extracts from technical producers, and the extraction is asymmetric: beneficiaries do not pay proportional costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_divergence,
    'How would the classification change if the continuity reading (organic medieval legitimacy across all domains) or rupture reading (total medieval corruption) were adopted instead of this hybrid bifurcation?',
    'Comparative analysis of the sibling constraint stories for latin_correctness__continuity_reading and latin_correctness__rupture_reading; examining whether their victim sets are empty (continuity) or total (rupture) versus the partial victim set documented here.',
    'Adoption of continuity would likely lower extractiveness and reclassify toward rope; adoption of rupture would expand victims to all medieval users and reclassify toward snare. The hybrid reading''s partial victim set is structurally distinctive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Structural divergence between sibling readings of the latin_correctness kernel.').

omega_variable(
    domain_bifurcation_naturalness,
    'Is the literary/technical divide in Latin norms a natural functional specialization or an artificially enforced status hierarchy?',
    'Cross-cultural sociolinguistic comparison of prestige allocation in diglossic or bilingual contexts; analysis of whether technical domains independently develop pragmatic registers in all linguistic traditions without the attendant status degradation seen here.',
    'If the divide is extrinsic status imposition, the constraint functions as extraction layered onto coordination; if intrinsic functional specialization, the extraction metric overstates the asymmetric cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_bifurcation_naturalness, conceptual, 'Whether the literary/technical legitimacy split reflects natural linguistic function or imposed status distinction.').

omega_variable(
    technical_practitioner_compliance_cost,
    'What measurable labor and educational costs did technical practitioners incur due to pressure to conform to classical norms in formal or dedicatory contexts?',
    'Historical analysis of technical manuscript prefaces, educational curricula for medical and legal students, and patronage letters assessing classical self-correction efforts and delays in publication.',
    'High documented compliance costs would confirm the victim structure and justify the moderate extractiveness score; negligible costs would suggest the constraint''s extraction is primarily symbolic rather than material.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technical_practitioner_compliance_cost, empirical, 'Quantification of concrete costs borne by technical practitioners under hybrid legitimacy norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_correctness_hybrid_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t10, latin_correctness__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t20, latin_correctness__hybrid_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t30, latin_correctness__hybrid_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t40, latin_correctness__hybrid_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t50, latin_correctness__hybrid_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(latin_correctness_hybrid_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(latin_correctness_hybrid_be_t10, latin_correctness__hybrid_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(latin_correctness_hybrid_be_t20, latin_correctness__hybrid_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(latin_correctness_hybrid_be_t30, latin_correctness__hybrid_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(latin_correctness_hybrid_be_t40, latin_correctness__hybrid_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(latin_correctness_hybrid_be_t50, latin_correctness__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(latin_correctness_hybrid_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(latin_correctness_hybrid_su_t10, latin_correctness__hybrid_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(latin_correctness_hybrid_su_t20, latin_correctness__hybrid_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(latin_correctness_hybrid_su_t30, latin_correctness__hybrid_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(latin_correctness_hybrid_su_t40, latin_correctness__hybrid_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(latin_correctness_hybrid_su_t50, latin_correctness__hybrid_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the latin_correctness kernel. The three stories form a constraint family: continuity_reading, hybrid_reading (this file), and rupture_reading. Each carries a distinct epsilon, beneficiary/victim structure, and classification derived from its specific structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
