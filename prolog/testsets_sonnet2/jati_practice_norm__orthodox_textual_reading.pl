% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Reading of Jati Boundaries as Fixed Varna Derivation
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This story instantiates the orthodox textual reading of the
 *   jati_practice_norm kernel: the claim that jati boundaries are direct,
 *   fixed derivations from a scriptural varna framework, such that deviation
 *   from an assigned occupational/ritual role constitutes ritual pollution
 *   rather than a social or economic choice. This is the reading in which the
 *   hierarchy presents itself as the most naturalized and least negotiable —
 *   it is asserted as textually immutable, not as locally negotiated (the
 *   localized_practice_reading) or as an artifact of colonial administrative
 *   categorization (the colonial_census_reading). Under this reading's own
 *   lights, the standing arrangement is the scripturally-grounded
 *   pollution/purity boundary as currently invoked and enforced by ritual
 *   authorities and dominant jatis; ε is authored for that arrangement, not
 *   for any egalitarian alternative.
 *
 * KEY AGENTS:
 *   - brahmin_ritual_authorities: institutional interpretive authority; sets and enforces the pollution boundary; collects ritual and material benefit
 *   - dominant_landholding_jatis: powerful secondary beneficiaries; command labor from stigmatized jatis under the framework's sanction
 *   - manual_scavenging_jatis, leatherworking_jatis, sanitation_labor_jatis: powerless targets; hereditary occupational assignment framed as ritually necessary, mobility blocked by structural and social sanction
 *   - reform_movements: excluded challengers whose counter-readings are not admitted into the sanctioned interpretive apparatus
 *   - comparative_religion_scholars: analytical observers documenting the textual tradition's actual historical variability and construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.88).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Reading of Jati Boundaries as Fixed Varna Derivation").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '56631a12-5888-4b6a-ba05-371a89197626').
narrative_ontology:cs_kernel_codification('56631a12-5888-4b6a-ba05-371a89197626', fixed_text).
narrative_ontology:cs_authority_grounding('56631a12-5888-4b6a-ba05-371a89197626', lineage).
narrative_ontology:cs_interpretation_layer_present('56631a12-5888-4b6a-ba05-371a89197626').
narrative_ontology:cs_reading_relation('56631a12-5888-4b6a-ba05-371a89197626', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_reading_relation('56631a12-5888-4b6a-ba05-371a89197626', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('56631a12-5888-4b6a-ba05-371a89197626', foundational, varna_scriptural_immutability).
narrative_ontology:cs_axiom_status(varna_scriptural_immutability, holdable).
narrative_ontology:cs_axiom_grounding('56631a12-5888-4b6a-ba05-371a89197626', varna_scriptural_immutability, theological).
narrative_ontology:cs_axiom('56631a12-5888-4b6a-ba05-371a89197626', foundational, ritual_pollution_ontological_reality).
narrative_ontology:cs_axiom_status(ritual_pollution_ontological_reality, holdable).
narrative_ontology:cs_axiom_grounding('56631a12-5888-4b6a-ba05-371a89197626', ritual_pollution_ontological_reality, theological).
narrative_ontology:cs_axiom('56631a12-5888-4b6a-ba05-371a89197626', secondary, occupational_deviation_as_cosmic_disorder).
narrative_ontology:cs_axiom_status(occupational_deviation_as_cosmic_disorder, holdable).
narrative_ontology:cs_axiom_grounding('56631a12-5888-4b6a-ba05-371a89197626', occupational_deviation_as_cosmic_disorder, deontological).
narrative_ontology:cs_reference_frame('56631a12-5888-4b6a-ba05-371a89197626', dharmashastra_varna_scriptural_mandate).
narrative_ontology:cs_drift_state('56631a12-5888-4b6a-ba05-371a89197626', post_constitutional_abolition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56631a12-5888-4b6a-ba05-371a89197626', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_landholding_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, leatherworking_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_scriptural_immutability).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_hierarchy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and administer the scriptural varna texts, adjudicate what counts as ritual pollution, and preside over purification rites. They occupy the apex of the hierarchy the framework describes, collect ritual fees and deference, and face no occupational assignment that could be read as polluting. They can reposition their own textual interpretations when politically convenient while insisting the framework itself is immutable for everyone else.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities, beneficiary).

% Occupy middle-to-upper positions in the varna-derived hierarchy that let them command labor from jatis assigned to polluting occupations at below-market terms, justified as the natural order rather than as extraction. They can migrate, diversify into commerce, or acquire land without incurring pollution stigma.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_landholding_jatis, beneficiary,
    powerful, generational, mobile, regional).

% Assigned by birth to occupations the orthodox framework designates ritually polluting (handling human waste, carcasses, cremation). They are barred from temples, wells, and commensality with higher jatis; attempting occupational mobility triggers social and sometimes physical retaliation framed as restoring cosmic/ritual order rather than as caste violence. Exit requires either state intervention, conversion, or migration to anonymity in a city — all costly and only partially effective, since jati identity travels with the person.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis, payer,
    powerless, biographical, trapped, local).

% Assigned hereditary occupations involving animal hides, classified as polluting under the orthodox reading. Occupational mobility is blocked by exclusion from land ownership, credit, and education historically justified by the same textual framework. Their labor is structurally necessary to the local economy while their persons are excluded from its ritual and social life.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, leatherworking_jatis, payer,
    powerless, biographical, trapped, local).

% Hereditarily assigned to sanitation work; the orthodox framework treats this assignment as scripturally ordained rather than as an artifact of who was made to do the work. Some individuals have gained constrained exit via urban migration and reservation-based education access, but the framework's persistence in home villages means family and marriage networks still enforce the boundary.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis, payer,
    powerless, biographical, constrained, local).

% Anti-caste and Bhakti-tradition reformers, and later Ambedkarite movements, contest the orthodox reading's claim that the varna-jati mapping is fixed scripture rather than a historically constructed and continuously reinforced hierarchy. They are excluded from the orthodox interpretive authority structure itself — their counter-readings circulate in parallel discourse but do not sit inside the ritual-authority seat that adjudicates pollution.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reform_movements, excluded,
    organized, generational, constrained, national).

% Study the textual sources (Manusmriti and related dharmashastra literature) and their historical redaction, noting substantial regional and temporal variation in how varna schema mapped onto actual jati practice. Their scholarship documents that the 'fixed' framework this reading asserts was itself compiled, edited, and selectively enforced over centuries.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The framework provides a shared cosmological and social ordering schema, assigning every group a named place, occupation, and set of ritual obligations, which in principle removes ambiguity about role and status within a large, otherwise uncoordinated agrarian society.
% TRANSFER_FUNCTION: Moves labor, ritual deference, and social standing from jatis assigned polluting occupations to jatis assigned priestly and dominant-landholding roles; also moves interpretive authority and its material perquisites (fees, land grants, deference) to the ritual-authority seat that administers the pollution/purity boundary.
% ABSENT_VOICES: The jatis assigned polluting occupations have no seat in the interpretive authority that decides what counts as pollution or how firmly the boundary is enforced; reform movements and the affected jatis' own oral and devotional counter-traditions are excluded from the sanctioned textual-interpretive apparatus even where they directly contest its claims.
% DISAPPEARANCE_RATIONALE: If the orthodox reading's authority to declare boundaries scripturally fixed and pollution ritually real were withdrawn, occupational assignment, marriage networks, temple access, and land relations built on the pollution/purity distinction would lose their sanctioned justification; social and legal mobilization against the boundary (already underway via constitutional and reservation mechanisms) would accelerate, and dominant jatis would lose a self-legitimating account for their position.
% FOUNDING_PROBLEM: Codifying occupational and ritual roles in a large, stratified agrarian society without a centralized bureaucratic state, using cosmological sanction to make the assigned order feel non-negotiable and therefore stable across generations.
% FOUNDING_PROBLEM_CORROBORATION: Ritual authorities attest the framework remains a live, scripturally required order. Comparative religion scholars, historians of the dharmashastra textual tradition, and the Indian constitutional and legal apparatus (Article 17 abolition of untouchability, extensive case law) attest from outside the beneficiary set that the 'fixed scriptural' claim is a historically constructed and variably enforced reading rather than an unchanging textual mandate, and that the founding problem it purports to solve is either dead or was never separable from the extraction it enables.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.85, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85) because the orthodox reading assigns entire hereditary groups to structurally necessary but stigmatized labor with blocked exit, and channels the resulting social and ritual capital to the interpretive authority and dominant jatis. Suppression is authored even higher (0.88) because the framework depends on continuous active enforcement — exclusion from temples, wells, commensality, and marriage networks, and social/physical retaliation against attempted mobility — not merely on passive tradition. Theater ratio rises over the measured interval (0.15 to 0.42) as legal abolition of untouchability and constitutional reservation policy push much of the enforcement into informal, denied, or performative registers (rituals maintained even where formal sanction is illegal) rather than open coercion. Accessibility collapse (0.7) reflects that once inside the framework's own interpretive logic, alternatives look like pollution rather than viable choices; resistance (0.72) reflects centuries of active anti-caste contestation, from Bhakti reformers through Ambedkarite movements to present-day legal mobilization.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual-authority seat, the boundary is a scripturally required order whose maintenance is a sacred duty, not an extraction. From the powerless payer seats, the identical structure computes as trapped, hereditary, coerced assignment to stigmatized labor with active retaliation against exit attempts. The engine should compute these as structurally divergent seat classifications from the same authored metrics — the divergence is the data, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin ritual authorities and dominant landholding jatis are declared beneficiaries: they collect deference, labor, and material benefit from the framework's operation and face no occupational stigma themselves, driving their derived directionality toward the beneficiary end. The three assigned-occupation jati groups are declared victims: hereditary assignment, blocked mobility, and active social/physical sanction against exit push their derived directionality toward the full-target end, amplified by their `trapped` or `constrained` exit options and `local` spatial scope where enforcement is most immediate and personal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading claims to solve — providing a stable, legible social and occupational order — is contested as either dead (a large-scale agrarian, non-bureaucratic society no longer needs cosmological sanction to coordinate labor) or never separable from the extraction it enabled. Classifying this specifically as the orthodox reading (rather than folding it into the localized-practice or colonial-census readings) prevents mislabeling a specific extractive-authority claim as either benign local coordination or as a purely externally-imposed administrative artifact — each of those is a structurally distinct claim with its own ε, and conflating them would average away the orthodox reading's distinctively high suppression and low interpretive contestability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_vs_historical_construction,
    'Is the varna-jati mapping this reading asserts as scripturally fixed actually a stable, continuous textual tradition, or a selectively compiled and periodically re-edited set of claims retrofitted to justify pre-existing social hierarchies?',
    'Philological and historical analysis of dharmashastra textual redaction history across regions and centuries, cross-referenced against archaeological and epigraphic evidence of actual occupational fluidity in different periods.',
    'If the textual tradition is shown to be substantially compiled/edited post hoc to ratify existing power arrangements, the orthodox reading''s core claim to fixity collapses, and the reading''s ε should be understood as measuring a legitimation exercise rather than a genuine natural-order description; this would sharpen the classification toward snare with lower ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fixity_vs_historical_construction, empirical, 'Whether the orthodox reading''s scriptural-fixity claim is a genuine stable textual tradition or a retrofitted legitimation.').

omega_variable(
    kernel_reading_authority_locus,
    'Given that all three readings (orthodox_textual, localized_practice, colonial_census) describe the same underlying jati boundary phenomenon, which reading''s authority structure actually held predominant enforcement power in a given historical period and region — and does the orthodox reading''s claim to primacy hold uniformly across the subcontinent, or only in regions/periods where Brahminical institutional power was strong?',
    'Comparative regional historiography documenting which enforcement mechanism (ritual-textual, locally-negotiated, or colonial-administrative) was dominant in specific times and places; this would show the orthodox reading is itself only one regionally/temporally bounded instantiation rather than a subcontinent-wide constant.',
    'If orthodox-textual enforcement was regionally and temporally patchy rather than uniform, this story''s claimed universal applicability should be scoped down, and the sibling readings would explain a larger share of the observed jati boundary phenomenon in regions/periods where ritual authority was weak.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_authority_locus, conceptual, 'Where and when the orthodox textual reading''s authority structure was actually the operative enforcement mechanism, versus the sibling readings.').

omega_variable(
    suppression_internalization_extent,
    'To what extent has the suppression enforcing this boundary become internalized (targets accepting the pollution framework as legitimate/inevitable) versus remaining purely externally coercive (social and physical retaliation)?',
    'Comparative sociological surveys of caste-consciousness and self-perceived legitimacy of the varna-purity framework among assigned-occupation jatis across regions with differing exposure to reform movements, legal enforcement of anti-discrimination law, and urbanization.',
    'A high internalized component would mean the constraint''s effective suppression persists even where formal/legal enforcement has weakened, since targets may reproduce the boundary through internalized stigma and endogamy even absent external threat — this changes what ''resolution'' would require beyond legal abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_extent, empirical, 'Structural vs. internalized suppression mechanism sustaining the pollution boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(jati_tr_t120, jati_practice_norm__orthodox_textual_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(jati_tr_t160, jati_practice_norm__orthodox_textual_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(jati_tr_t200, jati_practice_norm__orthodox_textual_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(jati_be_t120, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 120, 0.86).
narrative_ontology:measurement(jati_be_t160, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 160, 0.85).
narrative_ontology:measurement(jati_be_t200, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.87).
narrative_ontology:measurement(jati_su_t120, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(jati_su_t160, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 160, 0.87).
narrative_ontology:measurement(jati_su_t200, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'jati boundaries' under the jati_practice_norm kernel. orthodox_textual_reading (this story) claims fixed scriptural derivation with ritual-authority enforcement and the highest authored ε (0.85). localized_practice_reading claims jati boundaries are locally renegotiated coordination norms with substantially lower ε and no fixed textual mandate. colonial_census_reading claims jati categories were stabilized and reified through external colonial administrative apparatus, with a different agenda-setting authority (the colonial state rather than indigenous ritual authority) and its own distinct ε. All three describe the same natural-language phenomenon but are structurally distinct claims about its mechanism, authority locus, and degree of extraction; per the ε-invariance principle they are authored as separate constraint files linked here rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
