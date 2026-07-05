% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare Regime: Sentience-with-Constrained-Use Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the animal-status kernel:
 *   animals possess morally relevant interests grounded in sentience, but
 *   those interests constrain rather than prohibit human use. The reading's
 *   characteristic structure is the gratuitous/instrumental line — cruelty
 *   statutes protect against wanton suffering while carving broad exemptions
 *   for 'customary' commercial, research, and agricultural practices. This is
 *   a distinct constraint from the abolitionist reading (which would place
 *   all instrumentally-used animals in the victim set, forecloses no
 *   exemption structure) and the property reading (which recognizes no
 *   independent animal interest at all, only owner-imposed welfare limits).
 *   Each reading is authored as its own file with its own stable epsilon;
 *   this file does not average across them or describe their contest
 *   internally.
 *
 * KEY AGENTS:
 *   - livestock_industry_operators: primary beneficiary (organized/arbitrage) — captures the exemption structure
 *   - biomedical_research_institutions: beneficiary (institutional/arbitrage) — operates under justified-pain standard
 *   - animals_subject_to_instrumental_use: excluded from this reading's victim set by structural design — this exclusion is the reading's defining move
 *   - animals_subject_to_gratuitous_cruelty: the narrow victim class this reading does recognize
 *   - legislatures_and_regulatory_agencies: agenda_setter negotiating the exemption boundary under lobbying pressure from both sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare Regime: Sentience-with-Constrained-Use Reading").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'd19e1a01-79ea-4a64-b978-fcfceec5a1cd').
narrative_ontology:cs_kernel_codification('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', distributed).
narrative_ontology:cs_authority_grounding('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', distributed).
narrative_ontology:cs_reading_relation('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', foundational, sentience_grounds_interests_not_rights).
narrative_ontology:cs_axiom_status(sentience_grounds_interests_not_rights, holdable).
narrative_ontology:cs_axiom_grounding('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', sentience_grounds_interests_not_rights, deontological).
narrative_ontology:cs_axiom('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', foundational, instrumental_use_permissible_absent_gratuitous_harm).
narrative_ontology:cs_axiom_status(instrumental_use_permissible_absent_gratuitous_harm, holdable).
narrative_ontology:cs_axiom_grounding('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', instrumental_use_permissible_absent_gratuitous_harm, conventional).
narrative_ontology:cs_reference_frame('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', sentience_bounded_instrumentalism).
narrative_ontology:cs_drift_state('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', contemporary_welfare_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d19e1a01-79ea-4a64-b978-fcfceec5a1cd', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, livestock_industry_operators).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_subject_to_gratuitous_cruelty).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, humane_treatment_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate large-scale animal agriculture under welfare statutes that set minimum-treatment floors (space, slaughter method, veterinary care) but explicitly exempt standard industry practices — confinement, branding, castration, transport conditions — from cruelty statutes via 'customary farming practice' exemptions they lobbied to write. They benefit from the welfare framing because it legitimizes continued use while absorbing only the cost of the exemption-carved floor, not the cost of full interest-recognition.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, livestock_industry_operators, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, livestock_industry_operators, agenda_setter).

% Use animals in research under institutional review boards that weigh scientific necessity against animal welfare but do not require alternatives to be exhausted first. The welfare frame lets them continue procedures classified as 'justified pain' rather than prohibited harm, provided minimum housing and analgesia standards are met.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Purchase meat, dairy, and animal-tested goods produced under this regime. They benefit from lower prices than a rights-based regime would permit and can exit individually (going vegan, buying certified-humane) but exit does not change the regime, only their personal participation in it.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, national).

% Protected from cruelty that serves no economic or scientific purpose — malicious torture, neglect resulting in visible suffering the statute recognizes. They have no capacity to exit, no representation in the standard-setting process, and their protection is bounded entirely by whether their treatment can be classified as 'gratuitous' rather than 'instrumental.'
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_subject_to_gratuitous_cruelty, payer,
    powerless, immediate, trapped, national).

% The vast majority of animals used for food, research, and labor fall here — their confinement, slaughter, and use are the statute's exemption cases, not its violation cases. This population is structurally excluded from the victim category this reading recognizes precisely because their use is instrumental rather than gratuitous; the welfare framework's central move is drawing this line and it is drawn without their participation.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_subject_to_instrumental_use, excluded,
    powerless, immediate, trapped, national).

% Lobby to narrow the gratuitous/instrumental line and expand what counts as cruelty, but operate within a framework that presumes instrumental use is legitimate. They can move the line incrementally through legislation and litigation but cannot challenge the instrumental-use premise itself without exiting the welfare framework entirely for the abolitionist one.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Write and enforce the statutes that define the gratuitous/instrumental boundary, set exemption categories for 'customary practice,' and adjudicate enforcement actions. They receive lobbying pressure from industry to keep exemptions broad and from advocacy groups to narrow them, and the resulting statutory language is a negotiated product of both.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, legislatures_and_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, administrable legal standard distinguishing acceptable animal use from prohibited cruelty, allowing courts, regulators, and industry to operate with predictable rules rather than case-by-case moral adjudication of every use of an animal.
% TRANSFER_FUNCTION: Moves the cost of animal suffering associated with instrumental use (confinement, slaughter, research procedures) onto the animals themselves and away from producers, researchers, and consumers, while moving the narrower cost of gratuitous cruelty onto the individuals who commit it.
% ABSENT_VOICES: The animals themselves have no representational capacity in the standard-setting process; the line between 'gratuitous' and 'instrumental' is drawn entirely by human legislators responding to human economic and scientific interests. Animal welfare advocates are present but operate inside a framework that has already conceded the legitimacy of instrumental use before advocacy begins.
% DISAPPEARANCE_RATIONALE: If the welfare-reading statutes disappeared overnight, the legal vacuum would either default to the property reading (animals as unrestricted objects, removing even the gratuitous-cruelty floor) or force adoption of something closer to the abolitionist reading (no instrumental use without consent-analog). Either direction is a substantial rearrangement: industries currently operating under welfare compliance costs would either lose all constraint or face prohibition; advocacy infrastructure built around incremental welfare litigation would need to redirect entirely.
% FOUNDING_PROBLEM: Unrestrained animal cruelty in industrializing societies (slaughterhouse conditions, vivisection without anesthesia, urban draft-animal abuse) generated public revulsion and produced early anti-cruelty statutes; the founding problem was containing visible, gratuitous suffering that offended human moral sentiment without disrupting the economic uses animals were already put to.
% FOUNDING_PROBLEM_CORROBORATION: Historians of animal law and legal scholars outside the livestock and research lobbies attest that the original anti-cruelty statutes targeted highly visible, non-instrumental cruelty (public torture, wanton neglect) and that the exemption structure for 'customary practice' was added later, largely at industry request, to insulate standard commercial and research practices from the same scrutiny — meaning the founding problem (containing gratuitous cruelty) is substantially solved for that narrow category while a much larger category of suffering was carved out from the start rather than resolved.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.45, matching the expected structural delta, driven by the exemption architecture: the statutory floor is real (it does prohibit some conduct) but the carve-outs for customary practice absorb the overwhelming majority of animal suffering associated with instrumental use into the non-victim category. Suppression (0.58) reflects that the boundary is actively defended — industry lobbies to keep exemptions broad, and enforcement agencies rarely prosecute conduct that fits within a customary-practice exemption regardless of the animal's subjective experience. Theater ratio (0.42) captures that welfare certification and compliance programs perform attentiveness to animal interests while the exemption structure ensures the underlying practices continue largely unchanged — a rising theater ratio over the interval reflects growing welfare-labeling and certification programs that substitute visible compliance signaling for changes in the exemption boundary itself.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (industry, research, consumers), the regime reads as a workable, ethically-attentive rope: it constrains cruelty while permitting necessary use. From the seat of animals subject to instrumental use — who cannot report their own perspective and are represented here only structurally — the same regime is functionally indistinguishable from unconstrained use, since the exemption boundary is drawn precisely around what they experience. The engine computes these divergent seat classifications from the structural data; this story does not assert which seat's reading is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Livestock operators, research institutions, and consumers are beneficiaries: they capture the economic and scientific value of continued instrumental use while paying only the cost of the welfare floor, which is calibrated by the same actors who benefit from a narrow floor. Animals subject to gratuitous cruelty are victims in the narrowest sense the reading recognizes — trapped, powerless, immediate time horizon, no representation. Animals subject to instrumental use are deliberately modeled as excluded rather than victims: this is the reading's defining structural claim, and generating them as victims would collapse this story into the abolitionist reading rather than testing the welfare reading on its own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (containing gratuitous, non-instrumental cruelty) is largely solved for the narrow category it targets — public torture and wanton neglect are genuinely rare and prosecuted where visible. But the exemption structure that was added to protect customary economic practices was never a response to a solved problem; it was carved out from the start to keep the coordination function (predictable legal standard) from ever reaching the much larger instrumental-use population. Classifying this as tangled_rope rather than snare or rope reflects that both a genuine coordination function (a workable, administrable standard reducing visible cruelty) and asymmetric extraction (industry-negotiated exemptions insulating the bulk of animal suffering from the standard) are simultaneously present and mutually dependent — the coordination function is real, but it functions as legitimating cover for the exemption structure to the animals excluded from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gratuitous_instrumental_boundary_stability,
    'Is the line between ''gratuitous'' cruelty and ''instrumental'' use a principled moral distinction or an artifact of which practices happened to be economically entrenched when welfare statutes were drafted?',
    'Historical and comparative analysis of which practices were classified as customary-practice exemptions and whether that classification tracked any independent ethical criterion (e.g., necessity, proportionality) versus simply tracking existing industry practice at the time of statutory drafting.',
    'If the boundary tracks entrenched economic practice rather than a principled distinction, the welfare reading functions largely as legitimation for whatever uses were already occurring, and its extractiveness is understated by treating the exemption structure as a stable feature rather than a moving target set by lobbying power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gratuitous_instrumental_boundary_stability, conceptual, 'Whether the welfare reading''s central boundary is principled or economically contingent.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the welfare reading''s acceptance of instrumental use genuinely foreclose the abolitionist reading within a single legal framework, or can both operate as competing interpretive layers within the same statute (e.g., some jurisdictions recognizing limited rights-like protections for specific species while maintaining general instrumental-use permission)?',
    'Comparative legal analysis of jurisdictions with hybrid statutes (e.g., great ape personhood provisions coexisting with general livestock exemptions) to determine whether foreclosure is total or reading-relations should be qualified as partial/domain-specific.',
    'If hybrid coexistence is legally common, the forecloses relation to the abolitionist reading may overstate the logical incompatibility — some frameworks may hold both premises for different animal categories simultaneously, which would argue for a coexists_with or influences relation instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the forecloses relation to the abolitionist reading holds universally or only within a specific framework.').

omega_variable(
    welfare_certification_theater_growth,
    'Is the rising theater_ratio (welfare certification and labeling programs) actually improving animal outcomes at the margin, or is it purely a compliance-signaling response to consumer pressure that substitutes for exemption-structure reform?',
    'Longitudinal outcome data comparing certified-humane operations to standard exemption-compliant operations on measures independent of certification-program self-reporting (e.g., third-party veterinary audits, mortality and injury rates).',
    'If certification programs produce negligible outcome improvement over baseline exemption compliance, the rising theater_ratio confirms Goodhart drift — proxy compliance metrics replacing the substantive interest-protection the welfare reading claims to provide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_certification_theater_growth, empirical, 'Whether growing welfare certification reflects real improvement or metric substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_status__welfare_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(anim_tr_t16, animal_status__welfare_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(anim_tr_t24, animal_status__welfare_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(anim_tr_t32, animal_status__welfare_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anim_be_t8, animal_status__welfare_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(anim_be_t16, animal_status__welfare_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(anim_be_t24, animal_status__welfare_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(anim_be_t32, animal_status__welfare_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anim_su_t8, animal_status__welfare_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(anim_su_t16, animal_status__welfare_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(anim_su_t24, animal_status__welfare_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(anim_su_t32, animal_status__welfare_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'animal moral status,' per the ε-invariance principle. animal_status__property_reading treats animals as objects with no independent moral standing (lowest ε, extraction not measured against animal interests at all since none are recognized). animal_status__welfare_reading (this file) recognizes sentience-based interests bounded by an instrumental-use exemption structure (ε ~0.45). animal_status__abolitionist_reading recognizes animals as rights-holders precluding instrumental use, placing the entire instrumentally-used population in the victim set (highest ε). The three do not share one ε or one classification — they are linked via network edges, not merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
