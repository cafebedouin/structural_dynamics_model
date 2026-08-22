% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading of Vedic Social Prescription (Divinely Mandated Cosmic Hierarchy)
 *   domain: religious/social
 *
 * SUMMARY:
 *   This story instantiates the orthodox literal reading of the varna kernel:
 *   that Vedic texts (Purusha Sukta, later elaborated in Dharmashastra)
 *   prescribe a divinely ordained, birth-fixed social hierarchy of Brahmin,
 *   Kshatriya, Vaishya, and Shudra, with Dalit groups positioned entirely
 *   outside the scheme and subject to additional segregation. Under this
 *   reading the hierarchy is not metaphor and not colonial administrative
 *   artifact — it is cosmic law binding on lived social practice, with real
 *   occupational, marital, and ritual-access consequences enforced across
 *   millennia. This is a high-epsilon snare: the coordination story (stable
 *   role division for agrarian society) is real at the founding but has long
 *   outlived its functional necessity, and the enforcement machinery
 *   (purity/pollution sanctions, endogamy enforcement, literacy exclusion)
 *   persists chiefly to preserve Brahmin ritual and material advantage. The
 *   sibling readings — reformist_spiritual_reading (no prescriptive social
 *   content) and colonial_orientalist_reading (Vedic corpus as codified
 *   administrative 'Hindu law') — describe structurally different constraints
 *   with different epsilon values and different victim sets; they are not
 *   represented here.
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: agenda_setter/beneficiary (institutional/arbitrage) — controls interpretive authority and ritual fee extraction
 *   - shudra_laborers: primary payer (powerless/trapped) — bears occupational and ritual-access restriction
 *   - dalit_outcastes: primary payer (powerless/trapped) — bears the most severe segregation, positioned outside the scheme entirely
 *   - lower_caste_women: compounded payer (powerless/trapped) — caste plus gender restriction
 *   - kshatriya_rulers and vaishya_traders: secondary beneficiaries who also pay ritual tribute upward
 *   - reform_movements: excluded voice, historically pushed outside the tradition rather than accommodated within it
 *   - textual_scholars: analytical observer assessing textual layering and historical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading of Vedic Social Prescription (Divinely Mandated Cosmic Hierarchy)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '23e409f0-1c2d-49fe-a59e-c836305a68a4').
narrative_ontology:cs_kernel_codification('23e409f0-1c2d-49fe-a59e-c836305a68a4', fixed_text).
narrative_ontology:cs_authority_grounding('23e409f0-1c2d-49fe-a59e-c836305a68a4', lineage).
narrative_ontology:cs_interpretation_layer_present('23e409f0-1c2d-49fe-a59e-c836305a68a4').
narrative_ontology:cs_reading_relation('23e409f0-1c2d-49fe-a59e-c836305a68a4', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('23e409f0-1c2d-49fe-a59e-c836305a68a4', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('23e409f0-1c2d-49fe-a59e-c836305a68a4', foundational, varna_is_literal_cosmic_ontology).
narrative_ontology:cs_axiom_status(varna_is_literal_cosmic_ontology, holdable).
narrative_ontology:cs_axiom_grounding('23e409f0-1c2d-49fe-a59e-c836305a68a4', varna_is_literal_cosmic_ontology, theological).
narrative_ontology:cs_axiom('23e409f0-1c2d-49fe-a59e-c836305a68a4', secondary, birth_determines_ritual_eligibility).
narrative_ontology:cs_axiom_status(birth_determines_ritual_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('23e409f0-1c2d-49fe-a59e-c836305a68a4', birth_determines_ritual_eligibility, conventional).
narrative_ontology:cs_reference_frame('23e409f0-1c2d-49fe-a59e-c836305a68a4', purusha_sukta_cosmic_body_hierarchy).
narrative_ontology:cs_drift_state('23e409f0-1c2d-49fe-a59e-c836305a68a4', post_constitutional_abolition_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('23e409f0-1c2d-49fe-a59e-c836305a68a4', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laborers).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_outcastes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, lower_caste_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_rulers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_traders).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_traders).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, purusha_sukta_social_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls ritual authority, textual transmission, and interpretive access to Sanskrit scripture. Collects fees, land grants, and deference for officiating rites that only Brahmins are qualified to perform under this reading. Frames the hierarchy as revealed cosmic order (Purusha Sukta) rather than social arrangement, which forecloses argument by definition — to dispute the hierarchy is to dispute the cosmos, not a policy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood, beneficiary).

% Warrior-administrator caste whose temporal authority is legitimated by Brahmin ritual sanction in exchange for material patronage; benefits from the hierarchy's stability but is itself subordinate to Brahmin interpretive authority on matters of dharma.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_rulers, beneficiary,
    powerful, generational, constrained, regional).

% Merchant caste occupies a middling position: excluded from ritual and political authority but permitted commerce and property, and structurally above Shudras. Pays ritual fees to Brahmins and tribute to Kshatriyas while benefiting from a hierarchy that places others beneath it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_traders, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_traders, payer).

% Assigned by birth to service labor for the three higher varnas; scripturally barred from independent ritual performance, Vedic study, and property accumulation in the classical reading. Occupational role is fixed at birth; marriage outside the assigned group is prohibited and enforced through social and sometimes physical sanction. No scripturally sanctioned exit exists within the framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laborers, payer,
    powerless, biographical, trapped, regional).

% Positioned entirely outside the four-varna scheme (avarna) and subjected to segregation from water sources, temples, and residential space under the orthodox reading's purity-pollution logic. Bears the most severe restrictions of any group under this reading and has historically faced violent enforcement of boundary maintenance.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_outcastes, payer,
    powerless, biographical, trapped, regional).

% Bears compounded restriction: caste position plus gender-based exclusion from ritual literacy and property rights that this reading also grounds in scripture. Marriage and labor are both constrained by caste-endogamy rules this reading treats as divinely fixed.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, lower_caste_women, payer,
    powerless, biographical, trapped, regional).

% Bhakti, Buddhist, Jain, and later Ambedkarite and Arya Samaj movements contest the literal hierarchical reading, arguing for metaphorical or egalitarian interpretations. Historically marginalized from the orthodox interpretive apparatus and, in the case of Buddhism and Jainism, driven partly outside the Vedic fold entirely rather than accommodated within it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reform_movements, excluded,
    organized, generational, constrained, national).

% Philologists and historians of religion assess whether the literal social-prescription reading is textually dominant, later interpolation, or one reading among plural strata within the corpus itself; their findings feed but do not resolve the live theological and political dispute.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the varna scheme coordinates a functioning agrarian society by assigning stable, predictable occupational roles (priestly, martial/administrative, mercantile, laboring) so that ritual, governance, and production each have a dedicated, trained class rather than open competition for every role.
% TRANSFER_FUNCTION: Moves labor, ritual deference, land access, and marriage-market position from Shudra and Dalit groups (and from women within all but the highest strata) upward to the Brahmin priesthood and, secondarily, to Kshatriya and Vaishya groups — extraction is enforced through purity/pollution sanctions, endogamy rules, and exclusion from literacy and ritual participation rather than direct taxation.
% ABSENT_VOICES: Shudra and Dalit communities under this reading have no scripturally sanctioned voice in defining or revising the hierarchy that governs them — the texts this reading treats as authoritative were composed and transmitted by the caste that benefits from them. Reform and heterodox traditions (Buddhist, Jain, Bhakti, Ambedkarite) that contest the literal reading are treated by orthodox authority as departures from, not internal correctives to, the tradition.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading lost its social enforcement power overnight, occupational assignment by birth, endogamy enforcement, and ritual-access exclusion would lose their scriptural warrant; labor markets, marriage markets, and access to temples and public resources across large populations would reorganize substantially — this is precisely what has occurred unevenly since 19th-20th century reform and constitutional abolition of untouchability, and the incomplete character of that rearrangement is itself evidence the reading was load-bearing, not decorative.
% FOUNDING_PROBLEM: Ancient Vedic-era society faced the coordination problem of dividing ritual, martial, economic, and labor functions across a large agrarian population without a centralized bureaucratic state; assigning function by birth-lineage offered a low-negotiation-cost solution to role allocation and training transmission.
% FOUNDING_PROBLEM_CORROBORATION: Anti-caste scholars and activists (B.R. Ambedkar's 'Annihilation of Caste' being the paradigm case) attest from outside the Brahmin-beneficiary seat that the occupational-coordination problem the hierarchy purportedly solved has long since been superseded by modern economic and administrative organization, and that the hierarchy's persistence past that point is maintained by social sanction and ritual authority rather than functional necessity. Comparative historians of religion (external to any caste interest) corroborate that occupational rigidity by birth was never strictly followed in practice even in periods orthodox texts describe it as absolute, indicating the prescriptive claim exceeded lived function even at composition.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because under this reading the hierarchy transfers labor, ritual deference, land access, and marriage-market position systematically upward, and this transfer is textually grounded as cosmic mandate rather than negotiated arrangement — there is no scripturally sanctioned bargaining position for Shudra or Dalit groups. Suppression is authored even higher (0.88) because persistence depends on active enforcement: purity/pollution sanctions, endogamy policing, temple/water-source exclusion, and literacy prohibition, historically backed by social and physical coercion. Accessibility collapse is high (0.7) because the cosmic-order framing forecloses argument by definition within the orthodox tradition's own terms — disputing the hierarchy is disputing the nature of reality, not a policy choice, which is precisely what distinguishes this reading from the reformist one. Resistance is authored high (0.75) reflecting the sustained historical record of anti-caste movements (Buddhist and Jain heterodoxy, Bhakti egalitarianism, 19th-20th century reform, Ambedkarite abolition) contesting the reading from within and outside the tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood sits at the full-beneficiary end: institutional power, arbitrage-grade exit (control over interpretation itself is a form of exit from the constraint's costs), civilizational time horizon. Shudra laborers, Dalit outcastes, and lower-caste women sit at the full-target end: powerless, trapped exit (no scripturally sanctioned path out under this reading), biographical time horizon (each generation re-inherits the position at birth). Kshatriya and Vaishya groups occupy an intermediate position — secondary beneficiaries relative to Shudras/Dalits but themselves subordinate to Brahmin ritual authority, hence dual beneficiary/payer coding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem answer documents that a genuine coordination function existed at composition (agrarian role allocation without centralized bureaucracy) but is now dead — modern economic and administrative organization has long superseded birth-based occupational assignment as a coordination mechanism. The hierarchy's persistence past that point, sustained by ritual authority and social sanction rather than functional necessity, is exactly the signature classification should catch: a founding coordination problem that has been resolved by other means while the extractive apparatus built on top of it continues operating. This distinguishes the story from a pure Mountain claim (no natural-law immunity is available once the founding problem is shown dead) and from treating the hierarchy as merely theatrical (the extraction is real and ongoing, not decorative).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_stratified_textual_reading,
    'Do the Vedic corpus and later Dharmashastra texts present a single, unified, literally prescriptive social doctrine, or do they contain multiple historically stratified layers (some cosmological/metaphorical, some legalistic, some later interpolations) that the orthodox tradition has flattened into a single reading?',
    'Philological dating of textual strata (e.g., distinguishing Rigvedic Purusha Sukta from later Dharmashastra elaboration), cross-referencing with archaeological and epigraphic evidence of actual social mobility in different historical periods.',
    'If the literal social-prescription reading is a later interpolation or selective emphasis rather than the corpus''s dominant original content, this constraint''s claimed_type as a description of ''the Vedic corpus'' would need to be narrowed to describe a specific interpretive tradition (post-Vedic Brahminical orthodoxy) rather than the foundational texts themselves — though the reading remains valid as authored, since it is explicitly scoped as one reading, not a claim about the whole corpus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_stratified_textual_reading, empirical, 'Whether the literal hierarchical reading reflects the corpus''s original content or a later selective orthodox construction.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (orthodox_varna_reading) of the contested kernel vedic_corpus_social_prescription. The sibling readings are reformist_spiritual_reading (the corpus describes spiritual unity and metaphorical cosmology with no prescriptive social content) and colonial_orientalist_reading (the corpus constitutes unified, timeless ''Hindu law'' for administrative codification). Where is the disagreement actually located?',
    'The disagreement is located at the interpretive-authority layer: whether the Purusha Sukta''s cosmic-body metaphor (Brahmin as mouth, Kshatriya as arms, Vaishya as thighs, Shudra as feet) is (a) a literal social ontology binding on practice [this reading], (b) a metaphor for undifferentiated spiritual unity with no social prescription [reformist reading], or (c) raw material colonial administrators and orientalist scholars codified into a fixed legal-administrative system that did not previously exist in that codified form [colonial reading]. These are not three measurements of the same social fact but three different claims about what kind of text this is and what social structure (if any) it grounds.',
    'Adopting the reformist reading would produce near-zero extraction (the constraint would not exist as a social-prescription constraint at all). Adopting the colonial reading would relocate the beneficiary from the Brahmin priesthood to the colonial administrative apparatus and produce a different victim/beneficiary structure keyed to codification rather than divine mandate. This reading (orthodox_varna_reading) is authored independently with its own epsilon (0.86) reflecting the standing arrangement as this reading''s own lights see it — not averaged with or hedged against the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the three-way kernel disagreement at the interpretive-authority layer over the Purusha Sukta''s ontological status, per Rule 2 of the committer frame.').

omega_variable(
    reform_tradition_internality,
    'Are Bhakti, Buddhist, Jain, and Ambedkarite critiques of the varna hierarchy internal correctives to the Vedic tradition (implying the orthodox reading itself contains the seeds of its own reform) or external departures that the orthodox tradition correctly excludes as non-Vedic?',
    'Historical and theological analysis of whether reform movements claimed continuity with or explicit rejection of Vedic authority; textual analysis of Upanishadic material that some reform traditions cite as supporting a non-literal reading even within the orthodox canon.',
    'If reform critiques are internal, the orthodox_varna_reading''s claim to represent ''the'' Vedic tradition (rather than one strand within an internally contested tradition) weakens, though the reading remains authorable as one strand actually held by identifiable communities across history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_tradition_internality, conceptual, 'Whether reformist critique is internal or external to the tradition this reading claims to represent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vedi_tr_t0, projected).
narrative_ontology:measurement(vedi_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t500, projected).
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement_basis(vedi_tr_t1000, projected).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement_basis(vedi_tr_t1800, projected).
narrative_ontology:measurement(vedi_tr_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2500, 0.4).
narrative_ontology:measurement_basis(vedi_tr_t2500, observed).
narrative_ontology:measurement(vedi_tr_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 3000, 0.4).
narrative_ontology:measurement_basis(vedi_tr_t3000, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(vedi_be_t0, projected).
narrative_ontology:measurement(vedi_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.72).
narrative_ontology:measurement_basis(vedi_be_t500, projected).
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement_basis(vedi_be_t1000, projected).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1800, 0.85).
narrative_ontology:measurement_basis(vedi_be_t1800, projected).
narrative_ontology:measurement(vedi_be_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2500, 0.87).
narrative_ontology:measurement_basis(vedi_be_t2500, observed).
narrative_ontology:measurement(vedi_be_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 3000, 0.86).
narrative_ontology:measurement_basis(vedi_be_t3000, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(vedi_su_t0, projected).
narrative_ontology:measurement(vedi_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.68).
narrative_ontology:measurement_basis(vedi_su_t500, projected).
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.78).
narrative_ontology:measurement_basis(vedi_su_t1000, projected).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement_basis(vedi_su_t1800, projected).
narrative_ontology:measurement(vedi_su_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2500, 0.88).
narrative_ontology:measurement_basis(vedi_su_t2500, observed).
narrative_ontology:measurement(vedi_su_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 3000, 0.85).
narrative_ontology:measurement_basis(vedi_su_t3000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the Vedic corpus's social content' into structurally distinct constraints per the epsilon-invariance principle: orthodox_varna_reading (this file, high-epsilon snare — literal cosmic-mandate hierarchy with Brahmin beneficiary and Shudra/Dalit victims), reformist_spiritual_reading (near-zero epsilon — no prescriptive social content, metaphorical cosmology), and colonial_orientalist_reading (moderate epsilon, different beneficiary — colonial administrative codification of a supposedly unified 'Hindu law'). Each carries its own claimed_type, stakeholders, and epsilon; none is an average or hedge of the others. Linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
