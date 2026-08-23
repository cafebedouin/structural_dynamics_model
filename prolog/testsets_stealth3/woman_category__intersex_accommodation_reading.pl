% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Binary woman-category enforcement as assessed by the intersex-accommodation reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the intersex-accommodation reading of the
 *   woman-category commitment: biological sex is treated as a non-binary
 *   spectrum, and the category includes female-typical biology together with
 *   intersex variations that do not fit the male category. The story is ABOUT
 *   the standing arrangement that actually enforces the two-box category —
 *   civil sex registration, clinical normalization of atypical infant
 *   anatomy, and sports sex-eligibility verification — assessed by this
 *   reading's own lights; the accommodation regime this reading endorses is
 *   NOT the referent, so extraction is not hedged toward zero. The
 *   arrangement coordinates genuinely (registration, services, and
 *   competitive categories all read from it) and extracts asymmetrically
 *   through the same boundary: a population of roughly 0.02 to 1.7 percent
 *   depending on definition bears irreversible infant surgery, mandated
 *   athlete medication, public scrutiny of intimate medical data, and
 *   exclusion, while the administrative benefit flows to everyone else and
 *   the procedural gains concentrate in the medical professions that run
 *   normalization. Extraction is low-grade in most policy domains — the
 *   machinery barely contacts the boundary population there — and
 *   concentrated and severe in elite sport, where a testosterone threshold
 *   converts a category boundary into a medication mandate. Interval T0 to
 *   T60 maps to approximately 1965 to 2025. The claim and the metrics are
 *   independent authored facts: the claimed type states the structure this
 *   reading believes true of the standing arrangement; the metrics state what
 *   the arrangement's operation looks like from the boundary.
 *
 * KEY AGENTS:
 *   - intersex_children: primary target (powerless/trapped) — non-consensual normalization surgery and binary registration before consent is possible
 *   - intersex_women_athletes: primary target (moderate/constrained) — eligibility testing, mandated testosterone suppression, exclusion (the Semenya seat)
 *   - parents_of_intersex_infants: proxy decision-makers bearing the arrangement's costs (powerless/constrained)
 *   - elite_sports_governing_bodies: agenda-setter and beneficiary (institutional/arbitrage) — writes and enforces the eligibility line
 *   - binary_medical_establishment: beneficiary and agenda-setter (institutional/arbitrage) — runs normalization protocols, collects procedures and authority
 *   - women_with_female_typical_biology: beneficiary with sweep exposure (organized/constrained) — receives category benefits, exposed to verification
 *   - intersex_advocacy_organizations: excluded voice (moderate/identity_locked) — outside the rule-setting seats, locked into the contest
 *   - human_rights_adjudicators: analytical observer (institutional/analytical) — alternately upholds and condemns the machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.52).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.5).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Binary woman-category enforcement as assessed by the intersex-accommodation reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, 'e04368c1-750a-4934-bf88-bae362f590c1').
narrative_ontology:cs_kernel_codification('e04368c1-750a-4934-bf88-bae362f590c1', distributed).
narrative_ontology:cs_authority_grounding('e04368c1-750a-4934-bf88-bae362f590c1', distributed).
narrative_ontology:cs_reading_relation('e04368c1-750a-4934-bf88-bae362f590c1', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('e04368c1-750a-4934-bf88-bae362f590c1', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e04368c1-750a-4934-bf88-bae362f590c1', foundational, biological_sex_is_non_binary_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_non_binary_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('e04368c1-750a-4934-bf88-bae362f590c1', biological_sex_is_non_binary_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('e04368c1-750a-4934-bf88-bae362f590c1', foundational, membership_requires_no_bodily_alteration).
narrative_ontology:cs_axiom_status(membership_requires_no_bodily_alteration, holdable).
narrative_ontology:cs_axiom_grounding('e04368c1-750a-4934-bf88-bae362f590c1', membership_requires_no_bodily_alteration, deontological).
narrative_ontology:cs_reference_frame('e04368c1-750a-4934-bf88-bae362f590c1', spectrum_accommodating_membership).
narrative_ontology:cs_drift_state('e04368c1-750a-4934-bf88-bae362f590c1', contemporary_post_ecthr_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e04368c1-750a-4934-bf88-bae362f590c1', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, women_with_female_typical_biology).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, binary_medical_establishment).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_children).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_women_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, parents_of_intersex_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, women_with_female_typical_biology).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, binary_sex_dimorphism_doctrine).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, testosterone_eligibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born with variations of sex characteristics that do not fit either standard box. In many jurisdictions they undergo non-consensual genital or gonadal surgery and hormonal treatment in infancy or childhood so that their bodies and birth registration fit the binary; the decisions are made for them before they can speak, and the results — scarring, loss of sensation, sterilization in the most extensive procedures, an assigned sex that may not match their later identity — are permanent. Exit does not exist for them; the machinery reaches their bodies first and their consent never.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_children, payer,
    powerless, biographical, trapped, global).

% Women with differences of sex development competing in elite female categories. They must submit to eligibility testing and, where their natural testosterone exceeds the federation threshold, lower it medically for a sustained period before competing or leave the category; their intimate medical details become the object of public proceedings, and the leading litigant has been barred from her signature event for years. Their options are medication, changing events, retirement, or a decade of litigation — the biology the rules attach to cannot be changed.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_women_athletes, payer,
    moderate, biographical, constrained, global).

% Make proxy decisions about surgery and registration inside a clinical framing that presents early normalization as standard care. They sign the consents, live with the outcomes, and carry the lifelong consequences for their children. The alternative framing — watchful waiting with decisions deferred to consent — has only recently become available in a minority of health systems, so for most of the interval their choice set was the protocol or refusal against medical advice.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, parents_of_intersex_infants, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, parents_of_intersex_infants, agenda_setter).

% Write and administer the eligibility rules for female categories, including the DSD regulations that set a testosterone threshold for athletes with differences of sex development. They collect administrable, defensible category lines and the legitimacy of defending women's competition, and they control the rule-making agenda: they can revise thresholds, adopt open categories, or accommodate boundary cases without asking anyone's permission, though each option draws litigation and political fire.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies, beneficiary).

% The pediatric endocrinology, urology, and surgery professions that designed and administer infant normalization protocols. Normalization procedures generate clinical volume, reimbursement, and professional authority over the classification of atypical bodies, and the same professions set the clinical framing parents decide within. They can revise practice — and in some jurisdictions are doing so, shifting toward consent-deferred care — but the protocol tradition and the authority built on it are theirs.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, binary_medical_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, binary_medical_establishment, agenda_setter).

% The great majority of category members. They receive the category's coordination benefits — unambiguous registration, single-sex services and spaces, definable competitive categories — without bearing the boundary costs most of the time. Their exposure is the verification machinery itself: it does not respect phenotype, so any competitor can be swept into testing on a rival's suspicion, as the leading DSD case shows — a woman who lived unambiguously as female until a rival federation filed a complaint.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, women_with_female_typical_biology, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, women_with_female_typical_biology, payer).

% Intersex-led organizations and their allies. They were absent from the tables where the clinical protocols and federation rules were written and entered the process only through UN bodies, human-rights courts, and national legislatures. Their work is the boundary population's voice; they cannot leave the contest without dissolving themselves, and their standing in rule-making remains advisory rather than agenda-setting.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocacy_organizations, excluded,
    moderate, generational, identity_locked, global).

% Courts and treaty bodies — the European Court of Human Rights, Court of Arbitration for Sport panels, UN treaty bodies and special procedures. They do not run the arrangement; they adjudicate challenges to it, and their findings have alternately upheld and condemned the enforcement machinery, eroding its legitimacy in some forums while it persists in others.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, human_rights_adjudicators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, binary_medical_establishment).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, administrable rule for who counts as a woman across civil registration, clinical management, single-sex services, and sports categorization; the binary version solves the two-box sorting problem with a single biological test line and keeps competitive categories definable.
% TRANSFER_FUNCTION: Moves bodily autonomy and decision authority from people with atypical sex characteristics to the arrangement's administrators: infants' genital autonomy moves to surgical protocols via proxy consent; athletes' hormonal privacy and competitive standing move to federation eligibility rules; procedural volume and professional authority flow to the medical professions; administrable category boundaries flow to sports federations and registries.
% ABSENT_VOICES: Intersex children (who bear the primary costs before they can consent), intersex-led advocacy organizations (absent from the clinical and federation rule-setting seats where the machinery was designed; admitted only later through UN and litigation channels), and athletes with differences of sex development (eligibility rules written without their consultation). They sit outside the agenda-setting seats; their entry route has been courts and treaty bodies, not rule-making tables.
% DISAPPEARANCE_RATIONALE: Civil registration, sports eligibility, clinical pathways, and single-sex service allocation all read from the binary category arrangement; overnight removal would force immediate re-derivation of every sexed legal and administrative rule, so the world rearranges.
% FOUNDING_PROBLEM: Mid-twentieth-century administration and medicine needed every body sorted into a two-box sex registry, and competitive sport needed a defensible line around women's categories; the optimal-gender paradigm and systematic sex verification were built to make atypical bodies fit the boxes.
% FOUNDING_PROBLEM_CORROBORATION: Sports federations and clinical traditions attest the sorting problem is live — administrability and category fairness still require lines. Intersex-led organizations, the UN Special Rapporteur on torture and health reports, and human-rights courts attest from outside the benefiting parties that the machinery's founding premise — that atypical bodies must be altered or excluded to preserve two boxes — is obsolete; the 2013 UN report on involuntary surgery, the 2015 Malta Declaration, and the European Court of Human Rights' 2023 Semenya judgment are the corroborating record. No corroborating source outside the benefiting set attests that the enforcement machinery as built remains necessary.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52 is a concentrated-severe, broad-negligible profile compressed into one scalar: most governed people never contact the enforcement machinery, while the boundary population bears irreversible surgery, medication mandates, and career exclusion — the scalar sits mid-range because the extraction is real but narrow, and the commentary carries the distribution. Suppression 0.50 is structural, not interpersonal: legal registration, clinical authority, and federation rules close alternatives; the enforcement apparatus rose through the mid-century surgery paradigm and mass chromosome screening, was partially dismantled in the 1990s, was rebuilt in sport by the 2011 DSD regulations, and is now eroding under litigation. Theater 0.45 and rising: mass chromosome screening detected no impostors across four decades while sweeping in women with variations; the DSD rules apply only to women with atypical biology while their justification is category-wide fairness; enforcement now persists after the European Court of Human Rights found it incompatible with human-rights obligations, which is performative maintenance of a justification the courts have discredited. Accessibility_collapse 0.40: alternatives do not collapse — consent-deferred clinical protocols, third sex markers, and open-category proposals exist and are being enacted in a growing set of jurisdictions. Resistance 0.60: organized intersex advocacy, UN condemnation, national surgery restrictions, and a decade of athlete litigation. The claimed type is authored from structure — a real coordination function and an asymmetric extraction running through the same boundary, held up by active enforcement — not from the metric values. Receipt of gain concentrates in the medical establishment, which converted the boundary into six decades of reimbursable procedure and professional authority; the sport-side benefit is administrability and legitimacy rather than captured gain, so the receipt surface names the medical seat. Fixing cost is cheap relative to benefit: consent protections for infant care are demonstrably enactable at low material cost where legislatures act, and the sport-side fix is rule-writing inside the federations' existing power — the obstacles are professional inertia and political exposure, not material cost, against a large benefit in ended harm.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the federation and clinical seats the arrangement is an administrable classification they run and are professionally invested in; from the intersex seats the same boundary operates as bodily-integrity extraction they never consented to. The typical-women seat sits near the beneficiary pole with a sweep-exposure tail: the machinery's costs are mostly not theirs, but the machinery does not respect phenotype when it activates. The advocacy seat experiences the arrangement as a contest it is locked into. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Sports governing bodies and the medical establishment declare as beneficiaries with arbitrage-grade exit: they set or profit from the machinery and can restructure it, so their derived directionality sits near the beneficiary end. Women with female-typical biology declare as beneficiaries with constrained exit — genuine category benefits, occasional sweep costs — placing them near but not at the beneficiary pole. Intersex children, athletes with DSD, and parents declare as victims with trapped or constrained exit, placing them near the full-target end; the children's seat is the purest target in the story, since the arrangement acts on their bodies before any consent capacity exists. The advocacy seat is identity-locked: the organization's existence is fused to the boundary population's position, so exit would dissolve the agent rather than free it — institutional identity fusion, and the classification of that seat would change only if its constituency's boundary position did. Spatial scope is global, which amplifies effective extraction modestly at the target end because verification standards travel across jurisdictions with competition calendars and clinical training pipelines.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents both mislabels. A pure-coordination reading would miss why the machinery is litigated: the coordination function — determinate membership for registries, services, and sport — is real and would survive reform, but it does not explain the surgery schedules or the medication mandates, which serve the boundary's tidiness rather than any participant. A pure-extraction reading would miss why the arrangement persists without a visible capturer in most domains: the benefit is diffuse administrability, and the extraction is concentrated on a population too small to organize the politics that would end it. The founding problem — sorting every body into two boxes — is contested rather than dead: registries and sport still need lines, but the specific machinery built to make atypical bodies fit is increasingly adjudicated obsolete. Declaring the founding problem dead while the world still rearranges around the category would flag capture or zombie maintenance; the honest current state is contested, with the rising theater trajectory on the sport front marking the drift toward performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'This story is the intersex_accommodation_reading of the woman_category kernel, authored over the standing binary-enforcement arrangement: what would instantiating a sibling reading change structurally?',
    'Author the sibling stories over the same referent and compare: the sex_biology_reading relocates the boundary to typical chromosomal and anatomical biology and places the boundary population outside the category; the gender_identity_reading relocates membership to internal identity and drops biology-anchored enforcement from the category''s operation; each sibling authors its own extractiveness over the shared referent.',
    'Extractiveness, victim set, and type are all reading-indexed; comparing values across the three stories measures the readings, not one constraint — cross-reading differences locate the disagreement rather than adjudicate it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Committer structure: one reading of the woman_category kernel; sibling readings relocate the boundary and the victim set.').

omega_variable(
    disagreement_location_in_kernel,
    'Is the live disagreement between the readings located in where the category boundary sits, or in what enforcement may extract from boundary cases?',
    'Compare the siblings'' treatment of the same case population (athletes with differences of sex development, intersex infants): convergence on bodily-integrity limits with divergent membership rules locates the dispute in boundary placement; divergence on enforcement limits as well locates a second dispute axis in extraction ceilings.',
    'Determines whether this reading competes with its siblings on membership or on enforcement restraint, and therefore which structural edges carry the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_in_kernel, conceptual, 'Where the readings'' disagreement actually sits: boundary placement versus enforcement limits.').

omega_variable(
    sport_extraction_regime_specificity,
    'Is the concentrated high extraction in elite sport intrinsic to any biologically anchored category line, or specific to the testosterone-threshold enforcement regime chosen?',
    'Compare eligibility regimes that accommodate athletes with differences of sex development without mandated medication (open categories, no-questions participation lines): if competitive-fairness outcomes hold under accommodation, the extraction is regime-specific rather than intrinsic.',
    'If regime-specific, a spectrum-accommodating line could govern sport at near-zero extraction; if intrinsic, every biologically anchored line extracts from someone at the performance boundary and this reading''s sports-domain extraction floor stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sport_extraction_regime_specificity, empirical, 'Whether sport-side extraction is intrinsic to biologically anchored lines or an artifact of the testosterone regime.').

omega_variable(
    normalization_practice_trajectory,
    'Will non-consensual infant normalization practice decline toward negligible globally, or persist across jurisdictions lacking consent protections?',
    'Longitudinal clinical-registry data and tracking of national legislation establishing consent requirements for intersex infant surgery.',
    'Persistence keeps the medical front''s extraction structural and holds the arrangement''s extraction floor high; decline re-concentrates the remaining extraction almost entirely in sports eligibility enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_practice_trajectory, empirical, 'Trajectory of infant normalization practice across jurisdictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t10, woman_category__intersex_accommodation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(woma_tr_t30, woman_category__intersex_accommodation_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(woma_tr_t40, woman_category__intersex_accommodation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(woma_tr_t50, woman_category__intersex_accommodation_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(woma_tr_t60, woman_category__intersex_accommodation_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(woma_be_t10, woman_category__intersex_accommodation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(woma_be_t30, woman_category__intersex_accommodation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(woma_be_t40, woman_category__intersex_accommodation_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(woma_be_t50, woman_category__intersex_accommodation_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(woma_be_t60, woman_category__intersex_accommodation_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t10, woman_category__intersex_accommodation_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(woma_su_t30, woman_category__intersex_accommodation_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(woma_su_t40, woman_category__intersex_accommodation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(woma_su_t50, woman_category__intersex_accommodation_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(woma_su_t60, woman_category__intersex_accommodation_reading, suppression_requirement, 60, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three readings that author different extraction values and victim sets over a shared referent (the standing binary-enforcement arrangement): this intersex-accommodation story authors concentrated severe extraction at the boundary (infant normalization, testosterone mandates); the sex-biology story authors the same referent as largely legitimate boundary-keeping with minor edge costs; the gender-identity story authors the referent's biology-anchored machinery as misdirected from the category's actual basis. The three stories form one constraint family linked by network edges; extraction comparisons across them measure the readings, not one constraint. This reading's legal and clinical victories (surgery restrictions, human-rights rulings) change the legitimacy conditions under which pure binary enforcement operates, which is the influences edge to the sex-biology sibling; the gender-identity sibling remains a rival live position with no upstream-downstream causal structure, which is the coexists edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
