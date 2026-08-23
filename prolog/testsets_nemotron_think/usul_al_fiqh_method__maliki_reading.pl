% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Custom as Independent Sources
 *   domain: legal/religious/theoretical
 *
 * SUMMARY:
 *   The Maliki school of Islamic jurisprudence treats the 'amal (living
 *   practice) of the people of Medina as an independent source of law
 *   alongside hadith, validates maslaha mursala (public interest unrestricted
 *   by specific textual evidence) as a legislative principle, and integrates
 *   'urf (customary norm) where it does not contradict explicit text. This
 *   methodology elevates regional customary law to source status, creating a
 *   genuine coordination function — grounding law in social reality and
 *   enabling adaptation — while simultaneously extracting from universalist
 *   textualist approaches and subordinating non-Medinan regional practices.
 *   The constraint persists through active scholarly transmission, judicial
 *   application, and institutional embedding in Maliki-majority regions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.58).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.42).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Custom as Independent Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "legal/religious/theoretical").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '1bb1056e-6e2f-42b0-aa36-d1825df98c5f').
narrative_ontology:cs_kernel_codification('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', formalized).
narrative_ontology:cs_authority_grounding('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', lineage).
narrative_ontology:cs_interpretation_layer_present('1bb1056e-6e2f-42b0-aa36-d1825df98c5f').
narrative_ontology:cs_reading_relation('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', foundational, medinan_practice_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', medinan_practice_independent_source, conventional).
narrative_ontology:cs_axiom('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', foundational, maslaha_mursala_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', maslaha_mursala_valid_source, conventional).
narrative_ontology:cs_axiom('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', secondary, urf_integrated_where_not_contradicting_text).
narrative_ontology:cs_axiom_status(urf_integrated_where_not_contradicting_text, holdable).
narrative_ontology:cs_axiom_grounding('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', urf_integrated_where_not_contradicting_text, conventional).
narrative_ontology:cs_reference_frame('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', medinan_practice_as_living_sunna).
narrative_ontology:cs_drift_state('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', contemporary_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1bb1056e-6e2f-42b0-aa36-d1825df98c5f', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_customary_practitioners).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_medinan_regional_practices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, local_customary_practitioners).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, living_practice_as_legal_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_unrestricted_by_text).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, customary_norm_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit, interpret, and apply the Maliki methodology across North and West Africa, al-Andalus, and diaspora. They adjudicate which Medinan practices are authoritative, define maslaha mursala boundaries, and determine when 'urf contradicts text. Their authority derives from isnad chains linking to Medinan teachers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_jurists, agenda_setter,
    institutional, generational, constrained, regional).

% The community of Medina whose 'amal (practice) is treated as independent evidence of prophetic sunna. Their lived continuity from the Companion era gives their practice epistemic privilege over isolated hadith reports. They benefit structurally from this elevation but cannot easily exit the role — their identity is fused with being 'the people of the practice'.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_community, beneficiary,
    organized, civilizational, identity_locked, local).

% Communities across Maliki regions (Maghreb, West Africa, Sudan) whose 'urf (custom) gains legal recognition when not contradicting text. They benefit from having local norms integrated into fiqh, but also pay when their customs are overridden by Medinan practice or textual evidence. Exit means adopting another madhhab or secular law — costly but possible.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_customary_practitioners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, local_customary_practitioners, payer).

% Scholars and movements (including some Hanbali, Salafi, and modernist reformers) who argue legal derivation must rest on authenticated text alone. They bear the cost of having their methodological preference excluded within Maliki jurisdictions. They can exit by operating in non-Maliki zones or advocating codification, but lose influence where Maliki law governs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    organized, generational, mobile, global).

% Regional communities outside Medina whose customary practices receive secondary recognition — integrated as 'urf but subordinated to Medinan 'amal. They pay epistemic extraction: their lived practice is filtered through a Medinan benchmark. Exit means seeking recognition through other madhhabs or state law.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_medinan_regional_practices, payer,
    moderate, biographical, constrained, regional).

% Followers of the other three Sunni schools who maintain competing usul frameworks. They observe Maliki methodology as a live alternative in the jurisprudential marketplace. Their exit option is analytical — they engage from within their own frameworks without being subject to Maliki rules.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanafi_shafii_hanbali_adherents, observer,
    institutional, generational, analytical, global).

% Nation-state legal reformers in Maliki-majority countries (Morocco, Algeria, Mauritania, etc.) who codify family law, commercial law, constitutional provisions. They would object to the open-endedness of maslaha mursala and the priority of unwritten practice over statute, but are structurally excluded from the classical usul discourse. They must work within or around the Maliki framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, modern_state_codifiers, excluded,
    institutional, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grounds legal derivation in the lived social reality of the community that preserved the prophetic legacy, integrating custom and public interest so fiqh remains responsive to circumstances not explicitly addressed in text.
% TRANSFER_FUNCTION: Moves interpretive authority from exclusive reliance on authenticated hadith text to a triad of Medinan practice, unrestricted public interest, and validated custom. Benefits Medinan community and local customary practitioners; costs universalist textualists and non-Medinan regional practices whose norms are subordinated.
% ABSENT_VOICES: Non-Medinan regional communities whose customs are filtered through a Medinan benchmark; modern reformers (especially women's rights advocates) who seek codified statute over living practice; minority madhhab communities in Maliki regions who must litigate under Maliki rules.
% DISAPPEARANCE_RATIONALE: If the Maliki methodology vanished overnight, the legal systems of North and West Africa would lose their distinctive interpretive framework: Medinan practice would lose its epistemic privilege, maslaha mursala would cease to authorize non-textual legislation, and 'urf would revert to mere fact rather than legal source. The region would reorganize around codified statute, Hanbali-style textualism, or secular law — a structural rearrangement, not continuity.
% FOUNDING_PROBLEM: How to derive binding law for novel cases when authenticated textual sources are silent, while remaining tethered to the prophetic legacy embodied in the community that lived it — without granting unbounded discretion to individual jurists.
% FOUNDING_PROBLEM_CORROBORATION: Attested across the usul al-fiqh literature of all four schools: Shafi'i's Risala engages Maliki practice as a live rival; Hanafi usul works (e.g., Sarakhsi's Usul) debate maslaha and 'urf; Hanbali texts (Ibn Qudama) explicitly reject Maliki maslaha mursala. The problem is corroborated by the very existence of the inter-madhhab debate — it is not a Maliki self-assertion.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the structural privilege granted to Medinan practice over other regional practices and over pure textual derivation — the constraint extracts epistemic authority from universalist textualism and redistributes it to a specific community's living practice. Suppression (0.42) is moderate: alternatives (other madhhabs, textualist movements) exist and operate, but within the Maliki framework the methodology actively constrains textualist derivation. Theater ratio (0.28) is low-moderate: the coordination function (grounding law in lived reality, enabling responsive adaptation) is genuine and substantial, but performative elements exist in the ritualized invocation of 'Medinan consensus' to settle disputed points. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives persist but face structural friction. Measurements show gradual extraction accumulation and theater rise over the classical period (150-1200 AH / ~767-1786 CE) as the school institutionalized and codified.
 *
 * PERSPECTIVAL GAP:
 *   From the Maliki jurist seat, the constraint is a rope — genuine coordination solving the problem of textual silence. From the universalist textualist seat, it is a snare — suppressing text-based derivation in favor of an unfalsifiable 'living practice'. From the non-Medinan regional practitioner seat, it is a tangled rope — their customs gain recognition (coordination) but only through a Medinan filter (extraction). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki jurists (agenda_setter, institutional, constrained exit) administer the methodology and benefit from its authority. The Medinan community (beneficiary, organized, identity_locked) receives structural epistemic privilege — their practice is treated as sunna. Local customary practitioners (beneficiary/payer, moderate, constrained) gain recognition for their customs but remain subordinate to Medinan practice. Universalist textualists (payer, organized, mobile) bear the cost of methodological exclusion within Maliki zones. Non-Medinan regional practices (payer, moderate, constrained) are filtered through a Medinan benchmark. Other madhhab adherents (observer, institutional, analytical) engage from outside. Modern state codifiers (excluded, institutional, trapped) must work around the classical framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deriving law for novel cases while tethered to prophetic legacy) remains live — new circumstances (biotech, finance, digital rights) still require non-textual derivation. However, the coordination function has partially atrophied: modern nation-state codification has displaced living practice in many domains, and maslaha mursala now operates more as a legislative tool for state actors than a juristic principle. The constraint persists partly through institutional inertia (piton dynamics) but retains genuine coordination where classical fiqh still governs (family law, endowments).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Maliki methodology a distinct constraint with its own ε, or a parameterization of a shared usul al-fiqh kernel?',
    'Apply ε-invariance test: if measuring the constraint via Medinan practice weight yields ε≈0.58 but measuring via textual restriction yields ε≈0.2 (Hanafi) or ε≈0.1 (Hanbali), the label ''usul al-fiqh'' covers multiple constraints. Decompose into separate stories per reading.',
    'If decomposed, each reading gets its own classification trajectory; if unified, the ε value becomes observer-relative and the classification loses structural meaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel label conceals multiple ε-invariant constraints.').

omega_variable(
    medinan_practice_coordination_vs_extraction,
    'Does elevating Medinan ''amal to independent source status solve a genuine coordination problem (grounding law in the community that preserved the sunna) or extract epistemic rents for a specific regional community?',
    'Counterfactual: if Medina had not been the early scholarly center, would another community''s practice have been elevated? Compare with Hanafi treatment of Kufan practice (not elevated to independent source).',
    'If coordination, the constraint is rope/tangled_rope; if extraction, it trends toward snare. Determines whether the beneficiary declaration reflects genuine function or capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_coordination_vs_extraction, conceptual, 'Whether Medinan practice privilege is functional coordination or regional capture.').

omega_variable(
    maslaha_mursala_discretion_boundary,
    'Does maslaha mursala (unrestricted public interest) provide a structured coordination mechanism or an unbounded discretion channel for agenda-setters?',
    'Trace historical invocations: when maslaha mursala authorized new rulings, were they constrained by identifiable public interest criteria, or did they track ruler/jurist preference? Compare with Shafi''i''s maslaha restricted to textual objectives (maqasid).',
    'If unbounded discretion, extraction is higher than measured; if structured, the coordination function is more robust. Affects theater_ratio and extractiveness trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_discretion_boundary, empirical, 'Whether maslaha mursala operates as disciplined coordination or discretionary extraction.').

omega_variable(
    suppression_mechanism_textualism,
    'Is the suppression of universalist textualism within Maliki framework structural (institutional exclusion from judicial office, curriculum) or internalized (textualist scholars accept Maliki methodology as legitimate)?',
    'Survey textualist scholars operating in Maliki regions: do they argue for reform from within the framework, or reject the framework entirely? Track career trajectories.',
    'If internalized, effective suppression is higher than structural measure; if structural only, exit options for textualists are better than modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_textualism, empirical, 'Structural vs internalized suppression of textualist alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 150, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t150, usul_al_fiqh_method__maliki_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__maliki_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(usul_tr_t500, usul_al_fiqh_method__maliki_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__maliki_reading, theater_ratio, 700, 0.22).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.25).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__maliki_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t150, usul_al_fiqh_method__maliki_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__maliki_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement(usul_be_t500, usul_al_fiqh_method__maliki_reading, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__maliki_reading, base_extractiveness, 700, 0.52).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.55).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t150, usul_al_fiqh_method__maliki_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__maliki_reading, suppression_requirement, 300, 0.3).
narrative_ontology:measurement(usul_su_t500, usul_al_fiqh_method__maliki_reading, suppression_requirement, 500, 0.35).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__maliki_reading, suppression_requirement, 700, 0.38).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.4).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is the Maliki reading of the usul_al_fiqh_method kernel. The four readings form a constraint family linked by shared kernel but distinct ε values: Hanafi (expansive qiyas/istihsan) ε≈0.35, Shafi'i (systematized hadith hierarchy) ε≈0.25, Hanbali (maximal textual restriction) ε≈0.15, Maliki (living practice + maslaha + custom) ε≈0.58. The ε spread confirms these are distinct constraints per ε-invariance principle, not one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, organized, 0.2).
constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
