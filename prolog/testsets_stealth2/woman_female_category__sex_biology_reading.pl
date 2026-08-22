% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biology Reading of the Woman/Female Category: Biology-Keyed Classification Regime
 *   domain: political philosophy/bioethics/gender studies/law
 *
 * SUMMARY:
 *   This story instantiates the sex_biology_reading of the
 *   woman_female_category kernel: the rule that category membership — for
 *   prisons, shelters, sports, facilities, and legal sex — is determined by
 *   chromosomal sex, reproductive anatomy, and developmental biology. The
 *   standing arrangement under contest is the biology-keyed classification
 *   regime as it operates across jurisdictions; epsilon is authored for THAT
 *   arrangement BY THIS READING'S OWN LIGHTS, per the fixed epsilon-referent
 *   rule: the reading holds the regime protective of the class it governs and
 *   holds the exclusion of trans women definitional rather than extractive,
 *   so it authors a low base extractiveness (0.30) while its own structural
 *   declarations record who bears costs through the arrangement. The sibling
 *   readings share the referent and author different epsilon over it: the
 *   gender_identity_reading sees the same biology-keyed regime as deeply
 *   extractive of trans people; the hybrid_contextual_reading splits epsilon
 *   by context. Same referent, reading-indexed values — that spread is the
 *   corpus datum this family exists to take. The claim/metric gap is
 *   deliberate and load-bearing: the reading CLAIMS the category
 *   determination as natural law (sex is binary and immutable; the category
 *   tracks it), while the authored metrics describe a contested, actively
 *   enforced, beneficiary-bearing arrangement; the engine measures that
 *   divergence through the false-summit path. The kernel label also conceals
 *   a deeper decomposition: the biological fact of human sexual dimorphism is
 *   a natural fact with no parties and no extraction — a separate
 *   genuine-mountain story — while the classification RULE built on it has
 *   beneficiaries, cost-bearing seats, and enforcement machinery, which is
 *   this story.
 *
 * KEY AGENTS:
 *   - natal_females: primary beneficiary class (organized/generational) — receives single-sex provision and category-based protection; membership biologically fixed and non-resignable
 *   - trans_women: primary target class (moderate/constrained) — bears exclusion from female-category provisions; contests through advocacy and jurisdiction
 *   - intersex_women: enforcement-object seat (powerless/trapped) — bears eligibility testing and classification disputes disproportionately
 *   - trans_men: forced-classification seat (powerless/identity_locked) — counted in the female category against lived identity
 *   - gender_nonconforming_cis_women: dual-positioned seat (powerless) — holds the category's protections while absorbing door-side policing
 *   - legislatures_and_courts: agenda setters (institutional) — re-key the category by statute or interpretation
 *   - sports_governing_bodies and prison_shelter_operators: administering agenda setters (institutional) — apply the rule where it binds hardest
 *   - incarcerated_trans_women: excluded seat (powerless/trapped) — highest-stakes cost-bearers with least voice
 *   - bioethics_and_law_reform_bodies: analytical observer — maps positions, holds no placement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.3).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.65).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, mountain).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biology Reading of the Woman/Female Category: Biology-Keyed Classification Regime").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political philosophy/bioethics/gender studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).
domain_priors:emerges_naturally(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '182f807e-6f12-488b-adae-3396edd5aee6').
narrative_ontology:cs_kernel_codification('182f807e-6f12-488b-adae-3396edd5aee6', formalized).
narrative_ontology:cs_authority_grounding('182f807e-6f12-488b-adae-3396edd5aee6', expertise).
narrative_ontology:cs_interpretation_layer_present('182f807e-6f12-488b-adae-3396edd5aee6').
narrative_ontology:cs_reading_relation('182f807e-6f12-488b-adae-3396edd5aee6', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('182f807e-6f12-488b-adae-3396edd5aee6', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('182f807e-6f12-488b-adae-3396edd5aee6', foundational, category_membership_tracks_biological_sex).
narrative_ontology:cs_axiom_status(category_membership_tracks_biological_sex, holdable).
narrative_ontology:cs_axiom_grounding('182f807e-6f12-488b-adae-3396edd5aee6', category_membership_tracks_biological_sex, empirically_contingent).
narrative_ontology:cs_axiom('182f807e-6f12-488b-adae-3396edd5aee6', secondary, single_sex_provision_requires_biological_boundary).
narrative_ontology:cs_axiom_status(single_sex_provision_requires_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('182f807e-6f12-488b-adae-3396edd5aee6', single_sex_provision_requires_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('182f807e-6f12-488b-adae-3396edd5aee6', biology_keyed_category_settlement).
narrative_ontology:cs_drift_state('182f807e-6f12-488b-adae-3396edd5aee6', contemporary_self_id_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('182f807e-6f12-488b-adae-3396edd5aee6', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_men).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, incarcerated_trans_women).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, human_sexual_dimorphism).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_vulnerability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult human females whose access to prisons, shelters, sports categories, changing facilities, and sex-disaggregated services is organized around biological sex. They receive single-sex provision and category-based legal remedies; organized advocacy networks litigate and lobby to keep the category boundary keyed to biology. Their category membership is fixed by their bodies and cannot be resigned — and it is not a membership they seek to leave.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females, beneficiary,
    organized, generational, identity_locked, national).

% Women who were assigned male at birth and live as women. Under a biology-keyed rule they are classified male for access to female-only spaces, sports categories, and services, and they bear the cost of exclusion from provisions organized around the female category. Their options run through jurisdiction shopping toward self-ID regimes, separate or third provisions, or contesting the rule through advocacy and litigation; none of these restores access inside biology-keyed institutions.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    moderate, biographical, constrained, global).

% Men who were assigned female at birth. A biology-keyed rule counts them in the female category for prisons, shelters, and data collection regardless of their lived identity, so they are housed, recorded, and counted against it. Their bodies are the object the rule reads; changing jurisdiction does not change how the rule classifies them, and advocacy bandwidth for their specific position is thin.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_men, payer,
    powerless, biographical, identity_locked, global).

% Women with differences of sex development whose chromosomal, anatomical, or hormonal profiles do not fit the binary cleanly. Eligibility testing in sport and classification disputes in institutions fall on them disproportionately; historical sex-verification regimes screened large athlete populations and sanctioned almost exclusively them, some being required to alter their bodies to compete. They cannot exit the classification their bodies are measured against.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_women, payer,
    powerless, biographical, trapped, global).

% Natal women whose appearance or presentation is masculine enough to draw challenges at facility entrances and in single-sex settings. They hold the category's protections on paper but absorb a share of the door-side policing burden — being questioned, delayed, or refused at entrances maintained for the category. Their membership is not in doubt; the friction arrives at the point of enforcement.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women, payer).

% Enact the definitions and decide what sex means in equality law, prison placement, and service provision. They can re-key the category to biology, identity, or context by ordinary legislation or interpretation, and have done so differently across jurisdictions; each choice binds every downstream institution and is revisable by the same machinery that made it.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Set eligibility rules for female categories in competition and maintain the testing and review machinery that applies biological criteria — and, in some federations, hormone thresholds — to athletes. They revise the criteria periodically as science, litigation, and politics move, and they bear the reputational cost of each contested case.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Run the facilities where placement decisions bind hardest. They apply whatever classification rule the law gives them, carry the duty of care for everyone housed, and absorb the incident risk and litigation exposure of every placement controversy. They cannot opt out of classifying; their discretion is bounded by whatever definition the law supplies.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_shelter_operators, agenda_setter,
    institutional, biographical, constrained, national).

% Trans women serving sentences under placement rules they had no part in making. Under a biology-keyed rule they are housed in men's facilities, where documented assault risks are high. They are the highest-stakes cost-bearers in this dispute and have the least access of anyone to the consultations, hearings, and rule-making processes that determine their placement.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, incarcerated_trans_women, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, incarcerated_trans_women, payer).

% Law reform commissions, bioethics panels, and human-rights bodies that map the positions, commission evidence reviews, and draft framework options for legislatures and courts. They hold no placement decisions and collect no provision; their output is analysis that the deciding institutions cite or ignore.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, bioethics_and_law_reform_bodies, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutions need a determinate, administrable, hard-to-game criterion for allocating sex-segregated resources (prisons, shelters, changing facilities, sports categories) and for collecting sex-disaggregated data. Chromosomal, anatomical, and developmental biology supplies a stable observable that does not turn on self-report, and the resulting boundary coordinates protection for a class facing sex-patterned violence and disadvantage.
% TRANSFER_FUNCTION: Access, protection, and category membership flow to natal females. Exclusion from female-category provisions flows from trans women and — under eligibility testing — from intersex athletes. Classification burdens (testing, verification, door-side challenges, forced recording) fall on those whose biology or presentation is ambiguous relative to the binary. Placement and record decisions bind everyone whose classification the rule touches.
% ABSENT_VOICES: Incarcerated people on every side of the question — natal female inmates whose safety the rule is said to protect, and trans women housed in men's facilities under it — have the least voice in the consultations that set placement rules; the incarcerated trans woman appears in this story as an excluded seat for exactly this reason. Intersex people were historically subjected to sex-verification regimes without representation in designing them. Trans men are classified without a dedicated seat in most rule-making forums.
% DISAPPEARANCE_RATIONALE: If the biology-keyed rule vanished overnight, prisons, shelters, and sports bodies would re-key classification to identity or context within a planning cycle, sex-disaggregated data collection would reorganize around whatever marker replaced it, and the organized protection class would lose its category-based legal remedies and rebuild its politics around the successor boundary. The institutional landscape is arranged around this rule; its disappearance would force rearrangement, not continuity.
% FOUNDING_PROBLEM: Sex-patterned vulnerability: male-pattern violence against females, sex-based performance gaps in sport, and female-specific health, privacy, and dignity needs — the problems that motivated women's refuges, women's prisons, female sports categories, and legal sex classification in the first place.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is corroborated from outside the beneficiary set: criminological and public-health data on male-pattern violence, UN special-rapporteur reporting on violence against women and girls, and the sports-science performance literature — sources no party in this dispute seriously contests. What no outside source attests is that the biology-keyed category rule specifically, as opposed to some protection architecture, is the necessary solution: trans-rights organizations, several human-rights bodies, and the sibling readings attest the problem is real while disputing that this rule remains the answer. The corroboration therefore covers the founding problem but not the arrangement's continued monopoly on solving it.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, ExtMetricName, E),
    domain_priors:suppression_score(woman_female_category__sex_biology_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(woman_female_category__sex_biology_reading),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30 — low for a contested arrangement — because the reading's own lights hold the regime protective of its governed class and classify the exclusion of trans women as the price of a natural boundary rather than extraction; the reading does, however, acknowledge rising cost-bearing (intersex eligibility disputes, door-side challenges to gender-nonconforming women, trans men recorded against their identity), which is why the series climbs rather than sitting near zero. Suppression (0.65) is the arrangement's most dynamic metric: through the 1990s the boundary largely administered itself; from roughly 2004 onward — gender-recognition statutes, self-ID jurisdictions, institutional divergence — holding the biology-keyed boundary has required active legislation and litigation, and the suppression_requirement series tracks that enforcement hardening deliberately, because the narrative IS an enforcement-capacity story. Theater (0.22) stays low because the protective functions (refuges, placement, eligibility, disaggregated data) remain real, with a growing ritual component in boundary-verification practice. Accessibility_collapse (0.45): alternatives do not collapse — self-ID jurisdictions, hybrid frameworks, and third-space provisions persist and are institutionalized, so exit from the rule's logic is costly but available. Resistance (0.70): the arrangement faces the most organized sustained contest of any classification rule in current law. All three series run on one shared grid (1972-2025, eight points, all observed). Coalition note: the powerless seats (intersex women, trans men, incarcerated trans women) lack independent coalition infrastructure; their interests currently ride on the trans-advocacy coalition (the moderate seat), which contests chiefly on the exclusion axis rather than the testing or placement axes. Receipt-surface rationale: the gains demonstrably accrue to the natal-female seat (provision and remedies are what it receives), and fixing is cost-prohibitive relative to claimable benefit — the mechanism is ordinary legislation, but the benefit of any replacement is precisely what the three readings dispute, both sides are mobilized, and no coalition can fix the rule at a cost below the benefit it could reliably claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the natal-female seat the arrangement is protection: the boundary is what stands between the class and sex-patterned violence, and the low authored epsilon is that seat's honest experience. From the trans-woman seat the same structure operates as exclusion enforcement: the boundary is the mechanism that keeps her out of the category, and her seat computes high effective extraction despite the story's low story-level epsilon — that divergence is the measurement, not an error. The intersex seat meets the rule at its sharpest point (eligibility testing); the trans-male seat experiences it as forced classification; the gender-nonconforming seat as friction at the door. The agenda-setter seats experience the rule as administrable and revisable; the excluded incarcerated seat experiences it as total. Identity-lock dynamics differ by seat: for the protection class the category is constitutive of a political identity, so if protections were delivered by another architecture the seat's stake in THIS boundary would dissolve; for trans men the lock is bodily — no frame change alters how the rule reads them. The engine computes per-seat classifications from power, exit, and role data; the story-level claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: natal_females and gender_nonconforming_cis_women are declared beneficiaries and sit near the beneficiary end (low d) — their biologically fixed membership is identity-locked, but identity-lock amplifies extraction only for targets, not for declared beneficiaries. The victim declarations place trans_women, trans_men, intersex_women, and incarcerated_trans_women near the target end (high d): each bears costs through the same structure that delivers the protections, with exit constrained by biology (intersex women, trans men), by custody (incarcerated trans women), or by jurisdiction (trans women). No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the seat relationships directly. The agenda setters (legislatures_and_courts, sports_governing_bodies, prison_shelter_operators) are declared in neither array; their directionality comes from the power-atom fallback, which is appropriate for seats that set or apply the rule rather than gain or pay through its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification does two pieces of work here. First, it blocks the false-summit move: the reading presents the category determination as natural law, but natural facts have no beneficiaries, and this arrangement has a named protection class plus named cost-bearing seats — the false-summit path forces exactly the question the reading's rhetoric is built to foreclose: is this a fact the rule tracks, or a rule that identifiable agents benefit from maintaining? Second, it blocks the reverse error: a pure-extraction framing would erase the genuine coordination the arrangement performs — sex-patterned violence is real, refuges and placement rules do protective work, and the founding problem is corroborated from outside the beneficiary set. If the engine computes a hybrid coordination/extraction structure for this story, both halves are preserved: real coordination function, asymmetric cost-bearing through the same structure. Mandatrophy in the strict sense is not the live risk — the founding problem is not dead — but the R5 mismatch check still guards the arrangement's self-account: the problem is corroborated, the necessity of THIS rule for solving it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_kind_vs_constructed_rule,
    'Is the category boundary a natural biological fact that the classification rule merely tracks, or a constructed rule that identifiable agents benefit from maintaining?',
    'Comparative institutional analysis: do the protective functions (safety, privacy, fair competition, data quality) survive under alternative classification rules with comparable outcomes? The false-summit signature evaluation on this story is itself part of the resolution machinery.',
    'If constructed-with-beneficiaries, the mountain claim fails the false-summit evaluation and the arrangement classifies as hybrid coordination/extraction; if natural, the reading''s mountain claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_vs_constructed_rule, conceptual, 'Natural law versus constructed rule with beneficiaries — the false-summit crux of this story.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (sex_biology_reading) of the woman_female_category kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No empirical resolution — the readings are live positions held by different parties. The disagreement is located in the determination rule for category membership: biology versus self-identification versus context-split. Each sibling story authors its own epsilon over the shared referent.',
    'Under gender_identity_reading, the victim set becomes trans people and epsilon for the same referent is high; under hybrid_contextual_reading, epsilon splits by context. This story''s classification holds only within this reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and disagreement location.').

omega_variable(
    carceral_safety_evidence_base,
    'Does biology-keyed placement in prisons and shelters actually produce the natal-female safety outcomes the reading claims, relative to identity-keyed or contextual placement?',
    'Cross-jurisdiction natural experiments comparing incident rates under different placement rules, controlling for population composition and facility differences.',
    'If no differential protection is measurable, the arrangement''s coordination function weakens and the exclusion it imposes looks uncompensated; if measurable, the reading''s low authored epsilon is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carceral_safety_evidence_base, empirical, 'The reading''s central empirical claim: the safety payoff of biology-keyed placement in prisons and shelters.').

omega_variable(
    trans_women_male_category_risk,
    'What risk does the biology-keyed rule impose on trans women housed in male facilities, and does the arrangement internalize that cost through separate provision or externalize it onto the excluded seat?',
    'Placement-outcome data for trans women across facility types; policy analysis of separate-provision viability and actual uptake.',
    'High uncompensated risk on the excluded seat strengthens the extraction reading of the structure even by sympathetic lights; effective separate provision would support the reading''s boundary-cost characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_women_male_category_risk, empirical, 'Whether the cost the boundary imposes on trans women is internalized or externalized.').

omega_variable(
    intersex_boundary_strain,
    'What share of the rule''s enforcement burden — eligibility testing, classification disputes, required bodily alterations — falls on intersex people, and does the medical-exception interpretation absorb the binary''s strain or does the rule extract from them?',
    'Sports eligibility case data and medical classification practice review, quantifying who is tested, sanctioned, and required to alter their bodies to compete or be classified.',
    'A heavy intersex burden would show effective extraction higher than authored, concentrated on the least powerful seat; a genuinely absorbent medical exception would contain it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_boundary_strain, empirical, 'Where the binary''s enforcement actually lands.').

omega_variable(
    enforcement_ratchet_vs_oscillation,
    'Is the 2004-2025 hardening of enforcement a ratchet (permanent consolidation of the biology-keyed boundary) or an oscillation (reversible with electoral and institutional tides)?',
    'Cross-jurisdiction tracking of policy reversals over the next political cycle: do self-ID jurisdictions revert, do biology-keyed rulings hold across governments?',
    'A ratchet consolidates enforced boundary-keeping and keeps excluded-seat extraction high; oscillation points toward a hybrid or contextual settlement as the attractor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ratchet_vs_oscillation, empirical, 'Trajectory of the suppression_requirement series.').

omega_variable(
    protection_exclusivity_separability,
    'Is the protective function of single-sex provision separable from the exclusivity of the biological boundary — could safety, privacy, and fairness be delivered under hybrid or service-design architectures without the binary gate?',
    'Outcome evaluation of third-space provision, trauma-informed mixed provision, and context-split pilots against single-sex baselines on safety, privacy, and fairness measures.',
    'If separable, part of the boundary-keeping is exclusion riding on real protection and the extraction component rises; if inseparable, the reading''s protective claim is vindicated and the low authored epsilon holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_exclusivity_separability, conceptual, 'Separability of the protective function from the exclusivity of the boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 1972, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1972, woman_female_category__sex_biology_reading, theater_ratio, 1972, 0.1).
narrative_ontology:measurement_basis(woma_tr_t1972, observed).
narrative_ontology:measurement(woma_tr_t1980, woman_female_category__sex_biology_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(woma_tr_t1980, observed).
narrative_ontology:measurement(woma_tr_t1988, woman_female_category__sex_biology_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement_basis(woma_tr_t1988, observed).
narrative_ontology:measurement(woma_tr_t1996, woman_female_category__sex_biology_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement_basis(woma_tr_t1996, observed).
narrative_ontology:measurement(woma_tr_t2004, woman_female_category__sex_biology_reading, theater_ratio, 2004, 0.16).
narrative_ontology:measurement_basis(woma_tr_t2004, observed).
narrative_ontology:measurement(woma_tr_t2012, woman_female_category__sex_biology_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement_basis(woma_tr_t2012, observed).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__sex_biology_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement_basis(woma_tr_t2020, observed).
narrative_ontology:measurement(woma_tr_t2025, woman_female_category__sex_biology_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(woma_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t1972, woman_female_category__sex_biology_reading, base_extractiveness, 1972, 0.18).
narrative_ontology:measurement_basis(woma_be_t1972, observed).
narrative_ontology:measurement(woma_be_t1980, woman_female_category__sex_biology_reading, base_extractiveness, 1980, 0.19).
narrative_ontology:measurement_basis(woma_be_t1980, observed).
narrative_ontology:measurement(woma_be_t1988, woman_female_category__sex_biology_reading, base_extractiveness, 1988, 0.2).
narrative_ontology:measurement_basis(woma_be_t1988, observed).
narrative_ontology:measurement(woma_be_t1996, woman_female_category__sex_biology_reading, base_extractiveness, 1996, 0.22).
narrative_ontology:measurement_basis(woma_be_t1996, observed).
narrative_ontology:measurement(woma_be_t2004, woman_female_category__sex_biology_reading, base_extractiveness, 2004, 0.24).
narrative_ontology:measurement_basis(woma_be_t2004, observed).
narrative_ontology:measurement(woma_be_t2012, woman_female_category__sex_biology_reading, base_extractiveness, 2012, 0.26).
narrative_ontology:measurement_basis(woma_be_t2012, observed).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__sex_biology_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement_basis(woma_be_t2020, observed).
narrative_ontology:measurement(woma_be_t2025, woman_female_category__sex_biology_reading, base_extractiveness, 2025, 0.3).
narrative_ontology:measurement_basis(woma_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1972, woman_female_category__sex_biology_reading, suppression_requirement, 1972, 0.25).
narrative_ontology:measurement_basis(woma_su_t1972, observed).
narrative_ontology:measurement(woma_su_t1980, woman_female_category__sex_biology_reading, suppression_requirement, 1980, 0.27).
narrative_ontology:measurement_basis(woma_su_t1980, observed).
narrative_ontology:measurement(woma_su_t1988, woman_female_category__sex_biology_reading, suppression_requirement, 1988, 0.3).
narrative_ontology:measurement_basis(woma_su_t1988, observed).
narrative_ontology:measurement(woma_su_t1996, woman_female_category__sex_biology_reading, suppression_requirement, 1996, 0.34).
narrative_ontology:measurement_basis(woma_su_t1996, observed).
narrative_ontology:measurement(woma_su_t2004, woman_female_category__sex_biology_reading, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement_basis(woma_su_t2004, observed).
narrative_ontology:measurement(woma_su_t2012, woman_female_category__sex_biology_reading, suppression_requirement, 2012, 0.48).
narrative_ontology:measurement_basis(woma_su_t2012, observed).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__sex_biology_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement_basis(woma_su_t2020, observed).
narrative_ontology:measurement(woma_su_t2025, woman_female_category__sex_biology_reading, suppression_requirement, 2025, 0.65).
narrative_ontology:measurement_basis(woma_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, resource_allocation).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the woman_female_category kernel, three readings — this story (sex_biology_reading), woman_female_category__gender_identity_reading, and woman_female_category__hybrid_contextual_reading. All three share one epsilon referent: the standing biology-keyed classification arrangement. Each authors its own reading-indexed epsilon over it (this reading: 0.30, protective by its own lights; the gender-identity reading: high, seeing the same arrangement as exclusionary; the hybrid reading: context-split). Same referent, different values — that spread is the family's measurement, per the OQ-26 rule that epsilon is a property of a reading, not a topic. The deeper decomposition the colloquial label conceals: the biological fact of human sexual dimorphism is a natural fact with no parties and no extraction — a separate genuine-mountain story — while the classification RULE built on it has beneficiaries, cost-bearing seats, and enforcement machinery (this story). The family splits fact from rule; this story's edges run to both siblings, and the natural-fact story would sit upstream of all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
