% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion of Religious Marriage Practice (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story represents the 'exogenous override' reading of the
 *   1890 Manifesto, which formally ended the practice of plural marriage in
 *   The Church of Jesus Christ of Latter-day Saints (LDS Church). In this
 *   reading, the Manifesto is understood as a direct result of overwhelming
 *   federal coercion, not an internal theological evolution. The theological
 *   doctrine regarding plural marriage is considered unchanged, with only its
 *   practice suspended under duress. This perspective highlights the federal
 *   government as a beneficiary extracting institutional compliance, and LDS
 *   Church members as victims bearing the costs of doctrinal abandonment and
 *   a legitimacy crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion of Religious Marriage Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'b60d197e-62e2-44a6-9696-6e98f13dea8b').
narrative_ontology:cs_kernel_codification('b60d197e-62e2-44a6-9696-6e98f13dea8b', fixed_text).
narrative_ontology:cs_authority_grounding('b60d197e-62e2-44a6-9696-6e98f13dea8b', extraction).
narrative_ontology:cs_interpretation_layer_present('b60d197e-62e2-44a6-9696-6e98f13dea8b').
narrative_ontology:cs_reading_relation('b60d197e-62e2-44a6-9696-6e98f13dea8b', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('b60d197e-62e2-44a6-9696-6e98f13dea8b', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('b60d197e-62e2-44a6-9696-6e98f13dea8b', foundational, divine_command_of_plural_marriage_is_eternal).
narrative_ontology:cs_axiom_status(divine_command_of_plural_marriage_is_eternal, holdable).
narrative_ontology:cs_axiom_grounding('b60d197e-62e2-44a6-9696-6e98f13dea8b', divine_command_of_plural_marriage_is_eternal, theological).
narrative_ontology:cs_axiom('b60d197e-62e2-44a6-9696-6e98f13dea8b', foundational, secular_authority_cannot_override_divine_law).
narrative_ontology:cs_axiom_status(secular_authority_cannot_override_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('b60d197e-62e2-44a6-9696-6e98f13dea8b', secular_authority_cannot_override_divine_law, deontological).
narrative_ontology:cs_reference_frame('b60d197e-62e2-44a6-9696-6e98f13dea8b', divine_command_eternal_practice).
narrative_ontology:cs_drift_state('b60d197e-62e2-44a6-9696-6e98f13dea8b', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b60d197e-62e2-44a6-9696-6e98f13dea8b', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_american_society).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted legal and political pressure, including confiscation of property and disenfranchisement, to force the LDS Church to abandon plural marriage. Benefited from the perceived 'normalization' of the institution within American society.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Forced to abandon a deeply held religious practice under duress, leading to internal conflict, loss of property, and social ostracization. Many felt a profound betrayal of their spiritual commitments, experiencing a legitimacy crisis as the church capitulated.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_members, payer,
    powerless, biographical, identity_locked, global).

% Issued the Manifesto under extreme duress, suspending a core theological practice to preserve the institution from existential threat. Faced the impossible choice between doctrinal integrity and institutional survival, leading to a complex internal narrative of 'suspension' rather than 'reversal'.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, agenda_setter).

% Perceived the abandonment of plural marriage as a victory for 'civilized' norms and a sign of the LDS Church's assimilation. Benefited from the removal of a perceived social and moral anomaly.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_american_society, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto, under this reading, did not solve an internal coordination problem for the LDS Church but rather coordinated the Church's practices with the legal and social demands of the federal government and mainstream American society, albeit under duress.
% TRANSFER_FUNCTION: Transferred institutional autonomy and a core religious practice from the LDS Church to the federal government and mainstream society, in exchange for the Church's continued legal existence and the cessation of federal persecution.
% ABSENT_VOICES: Those members who felt the deepest spiritual commitment to plural marriage and viewed the Manifesto as a capitulation rather than a divine revelation. Their voices were marginalized within the institutional narrative, and many faced excommunication or left the Church.
% DISAPPEARANCE_RATIONALE: If the federal coercion and the Manifesto's effects vanished, the LDS Church would face an immediate and profound internal crisis regarding the status of plural marriage, potentially leading to schisms and a re-evaluation of its theological and historical narrative. The relationship between religious institutions and state power in the US would also be fundamentally re-evaluated.
% FOUNDING_PROBLEM: The federal government perceived the practice of plural marriage by the LDS Church as a violation of national laws and social norms, leading to a constitutional crisis and widespread persecution of the Church.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal legislative acts (e.g., Edmunds-Tucker Act), Supreme Court rulings (e.g., Reynolds v. United States), and contemporary journalistic accounts from outside the LDS Church consistently corroborate the federal government's view of plural marriage as an intractable problem requiring state intervention. The problem of federal persecution of the Church for this practice is now dead.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully forced the Church to abandon a core religious practice, fundamentally altering its institutional and theological landscape. Suppression is very high (0.90) due to the severe legal and political penalties imposed by the federal government, including property confiscation, imprisonment, and disenfranchisement, which left the Church with virtually no viable alternatives. The theater ratio is moderate-high (0.60) because the Church's public narrative of 'suspension' rather than 'reversal' served to maintain internal cohesion and theological continuity, even as the actual practice ceased under external pressure. Resistance was high (0.80) initially, but ultimately collapsed under the weight of federal enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary enforcement of law and social order. From the LDS Church leadership's perspective, it was a tragic but necessary capitulation to preserve the institution. From the perspective of many faithful members, it was a profound test of faith and a source of deep internal conflict, as the divine command to practice plural marriage was seemingly overridden by secular authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a clear beneficiary (d=0.0) as it achieved its policy goals and 'normalized' the LDS Church. LDS Church members and leadership are victims (d=1.0) as they bore the direct costs of abandoning a core practice and faced severe persecution. Mainstream American society is also a beneficiary (d=0.1) as its social norms were upheld.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Manifesto as a 'Rope' (pure coordination) or 'Scaffold' (temporary support) from the Church's perspective. The high extractiveness and suppression, coupled with the 'dead' founding problem status (from the federal perspective) and 'world_rearranges' disappearance verdict, firmly establish it as a 'Snare' from the perspective of the coerced institution. The theatrical element (narrative of suspension) is a coping mechanism for the snare, not a sign of a 'Piton' where function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_doctrine_status,
    'Was the theological doctrine of plural marriage truly ''suspended'' and ''unchanged'' by the Manifesto, or did the external coercion implicitly force a reinterpretation of the doctrine itself?',
    'Analysis of post-Manifesto theological discourse, changes in scriptural interpretation, and the long-term evolution of LDS marriage theology. If subsequent theological developments implicitly or explicitly reinterpreted the doctrine, it suggests more than mere suspension.',
    'If the doctrine was implicitly reinterpreted, the ''exogenous override'' reading''s claim of doctrinal stasis is weakened, potentially shifting the constraint''s classification towards a ''Tangled Rope'' where internal and external pressures co-created a new, albeit contested, theological framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_doctrine_status, conceptual, 'Ambiguity regarding the true status of plural marriage doctrine post-Manifesto.').

omega_variable(
    legitimacy_crisis_depth,
    'To what extent did the Manifesto create a lasting legitimacy crisis for the LDS Church among its members, particularly those who felt a strong commitment to plural marriage?',
    'Sociological studies of post-Manifesto dissent, formation of fundamentalist groups, and internal Church records detailing member disaffection or re-alignment. Quantitative analysis of membership retention and schismatic movements.',
    'A deeper and more widespread legitimacy crisis would amplify the ''victim'' status of LDS members and underscore the coercive nature of the constraint, reinforcing the ''Snare'' classification. A more limited crisis might suggest greater internal acceptance or a more effective institutional narrative of adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_crisis_depth, empirical, 'The extent of internal legitimacy crisis caused by the Manifesto.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the measured suppression primarily structural (legal penalties, property confiscation) or internalized (cognitive patterns of obedience, identity fusion with the Church that made exit unthinkable)?',
    'Post-exit suppression trajectory of dissenting groups: if suppression persists after the extractive mechanism is removed (e.g., excommunication leading to continued social isolation), reclassify as partially internalized. Historical accounts of individual choices and narratives of ''faith over practice'' vs. ''practice over institution''.',
    'If internalized suppression was significant, the constraint''s effective suppression is higher than the structural measure suggests — the target carried the suppression with them after exit, making the Snare more insidious. If purely structural, removal of external pressure would have led to immediate resurgence of practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of religious obedience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1862, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1862, 0.2).
narrative_ontology:measurement(marr_tr_t1870, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1870, 0.35).
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1880, 0.5).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1862, 0.6).
narrative_ontology:measurement(marr_be_t1870, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1870, 0.7).
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1880, 0.8).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1862, 0.7).
narrative_ontology:measurement(marr_su_t1870, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1870, 0.78).
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1880, 0.85).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. This 'exogenous override' reading emphasizes federal coercion and doctrinal suspension, contrasting with the 'endogenous reinterpretation' (divine revelation) and 'hybrid pragmatic' (strategic adaptation) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
