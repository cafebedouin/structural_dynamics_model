% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage as Ontologically Indissoluble under Hierarchical Adjudication
 *   domain: religious_doctrine/canon_law
 *
 * SUMMARY:
 *   This constraint instantiates the hierarchical_indissolubility_reading of
 *   the marriage_sacrament kernel. It treats marriage as an ontologically
 *   indissoluble reality whose validity and dissolution can only be
 *   adjudicated by the canonical hierarchy. Under this reading,
 *   indissolubility is not an ideal but a constitutive property of
 *   sacramental marriage; exit from an invalid marriage is possible only
 *   through a hierarchical tribunal declaration of nullity, and remarriage
 *   without such a declaration results in permanent exclusion from the
 *   Eucharist. The victim set is divorced and remarried Catholics who seek
 *   full sacramental participation. This reading is contested by a
 *   civic_pastoral_reading that treats indissolubility as aspirational and
 *   subject to pastoral discernment; that sibling reading is a separate
 *   constraint.
 *
 * KEY AGENTS:
 *   - canonical_hierarchy: Primary agenda-setter and beneficiary (institutional/global) â controls adjudication and collects institutional obedience and tribunal resources.
 *   - divorced_remarranged_catholics: Primary targets (powerless/identity_locked) â bear exclusion, costs, and delays.
 *   - parish_clergy: Enforcement layer (moderate/constrained) â administers denial of sacraments at the local level, bears pastoral cost.
 *   - doctrinally_aligned_lay_community: Coordinated beneficiaries (organized/constrained) â receive boundary clarity at the cost of their own mobility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.82).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.78).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage as Ontologically Indissoluble under Hierarchical Adjudication").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '6ef6aa85-c66d-4bac-99fd-5d1d0e302619').
narrative_ontology:cs_kernel_codification('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', formalized).
narrative_ontology:cs_authority_grounding('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', lineage).
narrative_ontology:cs_interpretation_layer_present('6ef6aa85-c66d-4bac-99fd-5d1d0e302619').
narrative_ontology:cs_reading_relation('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', marriage_sacrament__civic_pastoral_reading, forecloses).
narrative_ontology:cs_axiom('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', foundational, marriage_is_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_is_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', marriage_is_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', foundational, hierarchical_tribunal_monopoly).
narrative_ontology:cs_axiom_status(hierarchical_tribunal_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', hierarchical_tribunal_monopoly, conventional).
narrative_ontology:cs_reference_frame('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', sacramental_ontological_indissolubility).
narrative_ontology:cs_drift_state('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', contemporary_post_vatican_ii, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ef6aa85-c66d-4bac-99fd-5d1d0e302619', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, doctrinally_aligned_lay_community).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarranged_catholics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, parish_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and adjudicates the doctrine of sacramental indissolubility through tribunals and magisterial teaching. Derives institutional authority and boundary-control from being the sole legitimate interpreter of marital ontology. Collects tribunal fees and obedience. Cannot unilaterally abandon indissolubility without undermining its own lineage authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_hierarchy, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, canonical_hierarchy, beneficiary).

% Seek sacramental participation after civil divorce and remarriage. Must submit to lengthy, costly tribunal processes for annulment or remain permanently excluded from full communion. Their religious identity is often inseparable from family and community ties, making exit equivalent to excommunication from their own life-world.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarranged_catholics, payer,
    powerless, biographical, identity_locked, global).

% Enforce hierarchical directives by denying Eucharist to remarried parishioners and referring cases to tribunals. Bear the pastoral cost of exclusion, including ruptured trust with congregations. Cannot publicly dissent from tribunal outcomes or magisterial teaching without facing ecclesiastical censure.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, parish_clergy, payer,
    moderate, biographical, constrained, local).

% Receive the coordination benefit of clearly defined sacramental boundaries and marital ontology. Their communities are shielded from ambiguity about remarriage, but they indirectly depend on the exclusion of others to maintain the clarity of the norm.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, doctrinally_aligned_lay_community, beneficiary,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, canonical_hierarchy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates sacramental ontology by establishing marriage as an indissoluble, ontological reality that requires a single hierarchical adjudicator to determine validity, thereby unifying doctrine and practice across the global Church.
% TRANSFER_FUNCTION: Moves authority, time, and material resources (tribunal fees) from divorced and remarried Catholics to the canonical hierarchy, in exchange for access to sacramental participation; moves legitimacy and obedience from the laity to the magisterium.
% ABSENT_VOICES: Divorced Catholics who have left the Church entirely are structurally absent from tribunal processes; progressive theologians and pastoral advocates who would argue for compassionate discernment over ontological adjudication are marginalised within the magisterial conversation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, divorced and remarried Catholics would immediately regain full sacramental participation; the canonical hierarchy would lose a major instrument of boundary maintenance and doctrinal authority; tribunal systems would collapse; the Church's claim to be the unique interpreter of marital ontology would fracture.
% FOUNDING_PROBLEM: The need to secure sacramental unity and prevent the dissolution of marriage within the Christian community, ensuring that the marital bond reflects an ontological reality rather than a contractual arrangement dissolvable by human will.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the problem is live, citing secularization and family breakdown. Sociologists and divorced Catholics attest the founding problem has shifted: the arrangement now generates the suffering it was meant to prevent, and its persistence serves institutional control more than sacramental unity. External ecclesial historians note the doctrine's codification consolidated papal authority in the twelfth through sixteenth centuries, suggesting the problem was partly constructed by the solution.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the denial of the Eucharist is a severe spiritual and social sanction, supplemented by tribunal fees and indefinite delays. Suppression is high (0.78) because the constraint persists through active sacramental denial and the marginalization of pastoral dissent. Theater is moderate (0.45): the theological discourse is genuine, but a significant share of enforcement activity maintains hierarchical authority and boundary performance rather than marital ontology itself. Accessibility collapse is high (0.80) because once the Catholic sacramental framework is accepted, alternatives (remarriage plus communion) are logically impossible without hierarchical permission. Resistance is moderate (0.55) because pastoral resistance, underground communion practices, and theological dissent exist but are actively suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The canonical hierarchy experiences this constraint as guarding a sacred ontological boundary and securing unity; divorced Catholics experience it as an extractive barrier to spiritual participation. The parish clergy occupy a squeezed seat where they simultaneously enforce extraction and suffer its pastoral costs. The engine will compute divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy sits near the beneficiary end (d low): it subsidizes the constraint with enforcement labor but receives authority, fees, and obedience. Divorced Catholics sit near the full-target end (d high): they pay tribunal costs, suffer exclusion, and have identity-locked exit. Parish clergy sit mid-to-high (d ~0.6): they are structurally necessary to enforcement but do not capture gains; their exit is constrained by ordination vows and institutional dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy check, one might mislabel this as a scaffold (transitional discipline) or rope (pastoral coordination). The genealogy reveals the constraint's founding problemâprotecting sacramental unityâis contested: external observers argue the doctrine consolidated papal authority and that modern pastoral needs have superseded the medieval synthesis. The constraint persists with active enforcement despite contested obsolescence, which prevents scaffold classification and supports tangled_rope. The theater ratio (0.45) captures the performative maintenance of a doctrine whose practical function has shifted from protecting marriage to marking hierarchical boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tribunal_justice_vs_extraction,
    'Does the annulment tribunal process function primarily as a pastoral avenue for justice, or as a revenue-generating and obedience-securing mechanism?',
    'Comparative study of tribunal fee structures, processing times, and outcomes across jurisdictions; analysis of whether procedural delays track resource extraction or genuine judicial complexity.',
    'If fees and delays are structurally necessary for extraction, the coordination function is subordinate to the extractive one, pushing classification toward snare; if they are incidental overhead, the tangled rope framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_justice_vs_extraction, empirical, 'Whether tribunal processes are coordination overhead or extraction mechanism.').

omega_variable(
    ontological_claim_empirical_status,
    'Is the claim that marriage is an ontologically indissoluble reality empirically grounded (in nature or revelation) or conventionally constructed through canonical tradition?',
    'Historical-critical analysis of the development of the indissolubility doctrine; theological examination of whether the claim functions as natural law or as enacted church rule.',
    'If ontological, the constraint has a mountain-like element at its core; if purely conventional, the extraction is entirely socially constructed and the mountain claim is false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_claim_empirical_status, conceptual, 'Whether indissolubility is natural law or constructed doctrine.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the extraction sustained by internalized identity fusion (''being Catholic'' = ''accepting tribunal authority'') rather than by structural barriers alone?',
    'Ethnographic study of divorced Catholics'' exit trajectories: do they leave when structural barriers (geography, family pressure) are removed, or does identity fusion keep them submitting to tribunals?',
    'High identity lock amplifies effective extraction beyond structural measures and indicates the constraint operates partly as identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs internalized suppression in sacramental exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mshr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mshr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(mshr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(mshr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(mshr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(mshr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(mshr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(mshr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(mshr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(mshr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(mshr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(mshr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mshr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mshr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(mshr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(mshr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(mshr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(mshr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into two structurally distinct constraints: the hierarchical_indissolubility_reading (this file) and the civic_pastoral_reading (sibling). They share a doctrinal vocabulary but instantiate different constraints with different epsilon values, beneficiary structures, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
