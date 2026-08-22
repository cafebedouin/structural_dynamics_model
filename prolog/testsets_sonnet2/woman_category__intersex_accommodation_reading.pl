% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Intersex-Accommodation Reading of the 'Woman' Category Boundary
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested 'woman' category kernel:
 *   the intersex-accommodation reading, which holds that biological sex is
 *   better modeled as a spectrum than a strict binary, and that 'woman'
 *   properly includes typical female biology plus intersex and DSD variations
 *   that do not fit the male category. This reading is distinct from the
 *   sex-biology reading (strict chromosomal/anatomical binary) and the
 *   gender-identity reading (self-identification as the operative criterion)
 *   — those are separate constraints, not alternative measurements of this
 *   one. Under this reading, most policy domains see negligible extraction
 *   because the accommodation is largely uncontested (small affected
 *   population, low stakes). But in elite sport, where testosterone-linked
 *   performance thresholds are used to draw eligibility lines, the same
 *   spectrum-acknowledging logic becomes the vehicle for excluding specific
 *   women (the Caster Semenya case being paradigmatic) from competition —
 *   turning an inclusion doctrine into a gatekeeping one at the boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.42).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.55).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Intersex-Accommodation Reading of the 'Woman' Category Boundary").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, 'e3a52d54-08f6-41c2-84dd-c3baf7a398a4').
narrative_ontology:cs_kernel_codification('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', distributed).
narrative_ontology:cs_authority_grounding('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', distributed).
narrative_ontology:cs_reading_relation('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', foundational, biological_sex_is_a_spectrum_not_strict_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_a_spectrum_not_strict_binary, holdable).
narrative_ontology:cs_axiom_grounding('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', biological_sex_is_a_spectrum_not_strict_binary, empirically_contingent).
narrative_ontology:cs_axiom('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', secondary, category_membership_may_be_biologically_grounded_yet_admit_developmental_variation).
narrative_ontology:cs_axiom_status(category_membership_may_be_biologically_grounded_yet_admit_developmental_variation, holdable).
narrative_ontology:cs_axiom_grounding('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', category_membership_may_be_biologically_grounded_yet_admit_developmental_variation, conventional).
narrative_ontology:cs_created_at('e3a52d54-08f6-41c2-84dd-c3baf7a398a4', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, sport_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, binary_category_administrators).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, typical_female_competitors_in_excluded_events).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_and_dsd_women).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, women_with_naturally_elevated_testosterone).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, sex_is_a_spectrum_not_a_binary).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, reproductive_anatomy_admits_developmental_variation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have female-typical gender identity, upbringing, and legal sex, but biological variation (e.g. 46,XY DSD with androgen-driven traits) that places them outside the strict binary. Under this reading they are acknowledged as women in law and daily life, but in specific high-stakes domains (elite track and field) their biology is treated as disqualifying evidence rather than protected variation. They cannot alter their underlying physiology and have no venue that treats their case as ordinary rather than exceptional; testosterone-suppression mandates or event exclusion are the practical costs they bear.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_and_dsd_women, payer,
    powerless, biographical, trapped, global).

% A wider population than diagnosed DSD cases: women whose natural endocrine variation places them near or above policy thresholds without any intersex diagnosis. They bear medical scrutiny, public suspicion, and eligibility risk based on a biological trait they did not choose and generally did not know was policy-relevant until tested.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, women_with_naturally_elevated_testosterone, payer,
    powerless, biographical, trapped, global).

% Write and enforce eligibility rules for women's categories in elite competition. Use the spectrum-acknowledging framing to justify testing and thresholds as protecting the coherence of the women's category rather than as excluding people from womanhood outright. Retain the power to set, adjust, and enforce the line, and to characterize any given case as ordinary variation or disqualifying advantage.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sport_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Compete in events where testosterone-linked performance advantage is contested. Benefit from thresholds that exclude or regulate higher-testosterone competitors, gaining competitive opportunity and podium access they might not otherwise have. Have limited voice over where the line is drawn but receive the practical benefit of its current placement.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, typical_female_competitors_in_excluded_events, beneficiary,
    moderate, biographical, constrained, global).

% Administer sex-classified benefits, facilities, and legal categories outside sport (prisons, some medical and legal contexts). Benefit from a spectrum framing that lets them retain binary administrative categories for most purposes while carving out named exceptions for intersex cases, rather than having to rebuild the category from scratch.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, binary_category_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, binary_category_administrators, agenda_setter).

% Hold that gender identity, not biology, should determine category membership, and are not accommodated by a reading that keeps membership tethered to biological variation (even a widened, spectrum-inclusive one). Their preferred boundary criterion is absent from this reading's operative test.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Hold that 'woman' should track typical female biology strictly, treating intersex variation as a separate medical category rather than as inclusion within 'woman.' They object that acknowledging a spectrum dilutes the category's evidentiary clarity and are not accommodated by this reading's inclusion of atypical biology under the same label.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_realist_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate specific disputes (e.g. CAS rulings on testosterone regulations) by hearing evidence from governing bodies, athletes, and endocrinologists, and can force revision of where and how the line is drawn without themselves holding a stake in the outcome.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, courts_and_sports_arbitration_panels, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, sport_governing_bodies).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single category label ('woman') that can be applied consistently across law, medicine, and daily administration without requiring a separate legal category for every biological variation, while acknowledging that female-typical biology is not perfectly binary.
% TRANSFER_FUNCTION: In most policy domains the reading transfers little — inclusion is the default outcome and no material resource moves. In elite sport, it transfers competitive opportunity, prize money, and selection slots away from higher-testosterone or DSD competitors toward typical-female competitors, via eligibility thresholds justified as preserving fair competition within the acknowledged spectrum.
% ABSENT_VOICES: Gender identity advocates, who reject biology as the operative criterion altogether, and strict sex-realist advocates, who reject spectrum-based inclusion as definitionally incoherent, are both structurally outside this reading's own framework — they are heard in the broader kernel contest but not accommodated within this specific reading's boundary test.
% DISAPPEARANCE_RATIONALE: Outside elite sport, if this reading vanished the practical effect on most administrative categories would be small — few domains actively test biology at the margin. Within elite sport, its disappearance would immediately reopen eligibility disputes: governing bodies would have to adopt either a stricter biology-only threshold or an identity-based one, both of which would rearrange who competes in the affected events. The parties dispute which world we are actually in.
% FOUNDING_PROBLEM: Intersex and DSD conditions were historically either erased (forced into a strict binary with no accommodation) or pathologized (treated purely as medical anomalies outside the category of 'woman' altogether). This reading was built to give such people a coherent legal and social category membership without requiring a third sex category, while leaving room to regulate performance-relevant biological variation in contexts where it is claimed to matter.
% FOUNDING_PROBLEM_CORROBORATION: Intersex advocacy organizations and endocrinologists outside competitive sport attest the accommodation problem for legal and social recognition is largely solved by this reading and remains live mainly at the margins. Athletes subject to testosterone regulations, along with independent sports-law scholars and CAS arbitration records, attest that in elite sport the same reading has been repurposed as an eligibility-exclusion mechanism, with the founding accommodation problem effectively inverted into a gatekeeping one — corroboration here comes from arbitration rulings and athlete testimony, not from governing bodies themselves.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, contested).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is authored as moderate rather than high because this reading's effect is domain-dependent: near-zero extraction in ordinary legal/administrative contexts, substantially higher in elite sport where thresholds convert biological variation into competitive disqualification. Suppression (0.55) reflects the active enforcement apparatus in sport (mandatory testosterone testing, medical intervention requirements) needed to hold the boundary in the domain where it bites hardest; it is not scaled by scope in this account, per the raw-structural-property rule — it is authored directly from the observed enforcement mechanics. Accessibility collapse is moderate (0.4): alternatives to the current threshold-based test exist and are actively litigated (CAS proceedings), so collapse is far from complete. Resistance is high (0.72) precisely because affected athletes, sports-law scholars, and human rights bodies actively contest the sport-specific application even where they may accept the general accommodation framing elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   From the governing-body seat, this reading is coordination: a principled, biologically-literate way to keep women's categories meaningful while accommodating genuine variation. From the targeted-athlete seat, the identical rule structure operates as enforced exclusion — the spectrum acknowledgment is real, but its practical use in this domain is boundary-policing against a specific, non-consenting minority. The engine should compute these as structurally different experiences of the same authored facts, not as a dispute to be resolved by picking one side's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Sport governing bodies are the agenda-setters and structural beneficiaries: they administer the threshold, retain discretion over its calibration, and are shielded from the costs it imposes. Typical female competitors in threshold-regulated events are secondary beneficiaries — they gain competitive opportunity from exclusions they did not architect. Intersex/DSD women and women with naturally elevated testosterone are the targets: trapped by biology they cannot alter, bearing medical, competitive, and reputational costs. Binary-category administrators outside sport benefit from a low-friction accommodation that lets them avoid rebuilding categories elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (giving intersex/DSD people coherent category membership) is largely solved in ordinary civil contexts — the mandate there has arguably succeeded and could sunset into unremarkable legal fact. But in elite sport the same institutional machinery has been redirected toward a different, adversarial function (competitive gatekeeping) without a corresponding change in justification, which is the tangled-rope signature: genuine coordination (a workable, spectrum-literate category) co-existing with asymmetric extraction (specific athletes bearing disqualification costs) through the same rule structure, requiring active enforcement to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_dependent_epsilon_boundary,
    'Is the sharp difference in extraction between ordinary civil/legal contexts (low ε) and elite sport (high ε) evidence that this is really two constraints wearing one label, or is it a single constraint whose extraction is legitimately domain-modulated by genuine performance-relevant stakes?',
    'Compare whether the underlying boundary-drawing logic (which biological markers count as disqualifying variation) is the same test applied with different stakes, or a structurally different test invoked only in sport. If sport uses a fundamentally different operative criterion (performance-linked hormone thresholds) rather than the same category test, that would support decomposition into a separate sport-specific constraint.',
    'If decomposition is warranted, the sport-domain application should be split into its own constraint story with its own high ε, leaving this story''s ε closer to its low-domain baseline; if not, the domain-dependent single-story treatment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_dependent_epsilon_boundary, conceptual, 'Whether elite-sport application is the same constraint at higher stakes or a distinct constraint.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does acknowledging a biological spectrum genuinely leave room for the gender-identity reading to coexist (different criterion, different cases), or does the act of anchoring ''woman'' to biology at all — even a widened, spectrum-inclusive biology — structurally foreclose identity-based membership in any single legal framework?',
    'Examine jurisdictions or institutions that have attempted to hold both criteria simultaneously (biology-spectrum for sport, identity for civil registration) and assess whether this produces stable dual criteria or forces adjudicators to pick one as controlling in cases of conflict.',
    'If dual criteria prove unstable in practice, the relation to gender_identity_reading should be revised from coexists_with toward a more contested framing; if stable, coexists_with is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether biology-spectrum and identity-based readings can coexist within one legal system or structurally displace each other.').

omega_variable(
    consenting_authority_of_threshold_science,
    'Is the testosterone-linked performance threshold used in elite sport a settled empirical finding, or a contested policy choice dressed in biological language?',
    'Independent meta-analysis of the endocrinological performance-advantage literature, weighted against athlete and legal challenges (e.g., CAS proceedings, peer-reviewed critiques of the underlying studies).',
    'If the threshold science is contested rather than settled, extraction in the sport domain is better characterized as policy-driven exclusion dressed as biological necessity, which would raise the authored ε and suppression further; if well-supported, the current moderate-high values are appropriately calibrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consenting_authority_of_threshold_science, empirical, 'Whether the sport-domain threshold rests on settled science or contested inference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_category__intersex_accommodation_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(woma_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(woma_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(woma_be_t4, woman_category__intersex_accommodation_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(woma_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(woma_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t4, woman_category__intersex_accommodation_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(woma_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(woma_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the woman_category kernel. sex_biology_reading holds strict chromosomal/anatomical determination; gender_identity_reading holds self-identification as controlling; this story (intersex_accommodation_reading) holds a spectrum-based biological criterion that includes atypical female-typical variation. Each reading has its own ε, beneficiary/victim structure, and classification — they are linked here rather than merged because averaging or hedging across them would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
