% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Consociational Fragmentation of Marriage Authority (Federalist-Millet Reading)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   Marriage, divorce, and succession authority is deliberately fragmented
 *   across recognized religious communities, each governing its own family
 *   law under state enforcement, with the legislature declining to enact any
 *   uniform code. This file instantiates the federalist_millet_reading of the
 *   marriage_authority kernel: the fragmentation is read as a consociational
 *   anti-tyranny mechanism — an elite bargain in which every community
 *   receives a protected family-law domain so that no majority can impose its
 *   norms on minorities, and legislative paralysis is a stability feature
 *   rather than a defect. The claim/metric gap is deliberate and small here:
 *   the reading's own frame emphasizes coordination, while the authored
 *   metrics record the extraction half the bargain itself concedes
 *   (cross-category and dissenting individuals pay through the same structure
 *   that pays the beneficiaries) — hence a tangled_rope claim rather than the
 *   naive pure-rope self-description. Constraint-family note: the colloquial
 *   label 'personal law system' decomposes into five structurally distinct
 *   readings of one kernel, each a separate story with its own epsilon over
 *   the SAME standing arrangement: communal_autonomy_reading authors
 *   near-zero epsilon (tradition, not bargain), this reading authors
 *   low-moderate epsilon (~0.34, protective bargain with conceded gatekeeping
 *   costs), judicial_harmonization_reading authors moderate epsilon
 *   (manageable patchwork), gender_rights_reading authors high epsilon
 *   (women's costs centered), and secularist_reading authors moderate epsilon
 *   with a terminal sunset (transitional anomaly). They are linked via
 *   network edges because they contest the same arrangement and cite one
 *   another.
 *
 * KEY AGENTS:
 *   - - minority_communities: Primary beneficiary (organized/identity_locked) — protected from majoritarian family-law imposition; defends the arrangement
 *   - - majority_community_members: Secondary beneficiary (powerful/constrained) — keeps its own communal law; concedes family-law hegemony over others
 *   - - communal_religious_authorities: Gatekeeping beneficiary (institutional/identity_locked) — administers each segment's marriage regime and collects jurisdictional rents
 *   - - constitutional_legislature: Agenda setter (institutional/constrained) — holds formal power to unify or reform, maintains paralysis by electoral arithmetic
 *   - - apex_constitutional_court: Administrative agenda setter (institutional/constrained) — adjudicates within the plural system, patches seams case-by-case
 *   - - interfaith_couples: Primary payer (powerless/constrained) — bears the cost of nonconformity: exceptional routes, notice-period exposure, conversion pressure
 *   - - women_under_personal_law: Payer (powerless/identity_locked) — bears gendered costs of communal law; reform routed through institutions they do not control
 *   - - unaffiliated_individuals: Excluded voice (powerless/trapped) — no communal box, no representative, liminal marital capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.34).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Consociational Fragmentation of Marriage Authority (Federalist-Millet Reading)").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'd52887b9-b209-41d4-b2c5-cbabd3b286bb').
narrative_ontology:cs_kernel_codification('d52887b9-b209-41d4-b2c5-cbabd3b286bb', formalized).
narrative_ontology:cs_authority_grounding('d52887b9-b209-41d4-b2c5-cbabd3b286bb', lineage).
narrative_ontology:cs_interpretation_layer_present('d52887b9-b209-41d4-b2c5-cbabd3b286bb').
narrative_ontology:cs_reading_relation('d52887b9-b209-41d4-b2c5-cbabd3b286bb', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d52887b9-b209-41d4-b2c5-cbabd3b286bb', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('d52887b9-b209-41d4-b2c5-cbabd3b286bb', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('d52887b9-b209-41d4-b2c5-cbabd3b286bb', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('d52887b9-b209-41d4-b2c5-cbabd3b286bb', foundational, fragmented_authority_prevents_majoritarian_tyranny).
narrative_ontology:cs_axiom_status(fragmented_authority_prevents_majoritarian_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('d52887b9-b209-41d4-b2c5-cbabd3b286bb', fragmented_authority_prevents_majoritarian_tyranny, instrumental).
narrative_ontology:cs_axiom('d52887b9-b209-41d4-b2c5-cbabd3b286bb', foundational, intercommunal_peace_outweighs_intra_segment_uniformity).
narrative_ontology:cs_axiom_status(intercommunal_peace_outweighs_intra_segment_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('d52887b9-b209-41d4-b2c5-cbabd3b286bb', intercommunal_peace_outweighs_intra_segment_uniformity, deontological).
narrative_ontology:cs_reference_frame('d52887b9-b209-41d4-b2c5-cbabd3b286bb', founding_consociational_compromise).
narrative_ontology:cs_drift_state('d52887b9-b209-41d4-b2c5-cbabd3b286bb', contemporary_majoritarian_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d52887b9-b209-41d4-b2c5-cbabd3b286bb', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, women_under_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_anti_tyranny_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, segmental_family_law_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious minorities living under their own marriage, divorce, and succession law, enforced by state courts. The arrangement guarantees that family law cannot be rewritten by majority vote. Membership is ascriptive: birth assigns the law, and leaving the community means losing the legal shield along with family and social world.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, identity_locked, national).

% Members of the largest religious community, who live under their own traditional family law and forgo the ability to legislate a uniform code for everyone. They retain demographic and political dominance in every other domain; in family law specifically they hold no power over other communities' norms.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community_members, beneficiary,
    powerful, generational, constrained, national).

% Clerical councils and personal-law boards that administer marriage, divorce, and annulment within each community. Every registration and every dissolution passes through their offices; reform proposals must be routed through them. They staff the tribunals the state empowers and collect the deference and jurisdiction that come with the monopoly.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_religious_authorities, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_religious_authorities, agenda_setter).

% Holds formal power to enact a uniform family code or to reform any personal law, and has declined to use it for the life of the arrangement. Family-law bills die in committee; the leadership calculates that touching any community's law costs more votes than it wins. The paralysis is maintained by electoral arithmetic, not legal incapacity.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_legislature, agenda_setter,
    institutional, biographical, constrained, national).

% Adjudicates disputes under the plural system: applies each community's statute, occasionally narrows a harsh rule by invoking constitutional guarantees in individual cases, and leaves the architecture intact. Each ruling patches one seam; the court has never undertaken systematic revision.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% Couples spanning two communities find no routine marriage path: each personal law governs only its own members, so they must use the exceptional civil route (with public notice periods that invite family and community interference), marry abroad, or have one partner convert. Costs fall on them individually; no institutional seat represents them.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, interfaith_couples, payer,
    powerless, biographical, constrained, national).

% Women whose divorce, maintenance, custody, and remarriage rights are defined by their community's law. Equalizing reforms proposed from outside are framed as attacks on the community; reform from inside runs through institutions women do not control. Exit — leaving the community — carries total social cost.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_under_personal_law, payer,
    powerless, biographical, identity_locked, national).

% People with no communal affiliation — apostates, atheists, children of mixed marriages who belong nowhere — for whom the segmental map has no box. Their marital capacity is liminal in every direction, and no institution speaks for them because the arrangement recognizes communities, not persons.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, unaffiliated_individuals, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, communal_religious_authorities).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes family law from majoritarian contest by assigning marriage, divorce, and succession jurisdiction to each recognized community under state enforcement: no community can impose its norms on another, and every segment holds a protected domain. Solves the heterogeneous-polity problem of legislating marriage without cultural surrender by any group.
% TRANSFER_FUNCTION: Moves marital jurisdiction and gatekeeping authority to communal elites; moves the costs of cross-category and dissenting individuals (interfaith couples, the unaffiliated, gender-dissenting members) onto those individuals; moves reform initiative away from the legislature, freezing the bargain's terms.
% ABSENT_VOICES: Individuals are represented only as community members: the woman seeking divorce, the interfaith couple, the apostate have no seat — the arrangement's units are communities, so intra-community dissenters are spoken for by the very authorities whose jurisdiction they contest. They are present in litigation and press but absent from the bargain's table, where renewal happens communally.
% DISAPPEARANCE_RATIONALE: Overnight removal would force immediate family-law unification under whichever coalition holds the legislature — majority norms imposed on minorities, communal tribunals dissolved, interfaith couples newly routable but through a code written without them, and communal elites mobilizing against the state. The intercommunal settlement in family law is load-bearing for the wider constitutional peace; its removal rearranges the polity, not just the registry.
% FOUNDING_PROBLEM: At state formation, religious communities feared majoritarian imposition of family law; the framers needed a formula to hold a heterogeneous polity together without forcing cultural surrender on marriage. The segmental carve-out was the price of consent to the constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Constituent-assembly and mandate-era records corroborate that the founding problem — majority imposition of family law on unwilling communities — was real and priced into the founding bargain. On current status, corroboration splits along the beneficiary line: minority-rights monitors and comparative-consociational scholarship, outside the benefiting parties, attest the threat remains live where majoritarian politics is resurgent; civil-liberties litigators and women's-movement documentation, also outside the benefiting parties, attest the stated problem has been overtaken by the arrangement's own gatekeeping costs. No single outside source settles liveness; the dispute is itself the signal.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.34: real but bounded — costs concentrate on cross-category individuals and women under restrictive communal rules, while the arrangement's insurance value is broad. Suppression 0.62, unscaled per the framework: the machinery is structural — absence of a routine civil route, notice regimes that expose couples to family and community interference, policing of boundary-crossing conversions — and it must stay active because exits exist and are used (marriages abroad, exceptional civil acts). Theater 0.30: adjudication and registration are real work, but a growing share of activity is performative — perpetual reform committees, consultative bodies, and anniversary defenses of pluralism that produce no revision. Accessibility_collapse 0.48: alternatives neither vanish nor come cheap; understanding the system reveals exits that are visible, costly, and increasingly policed. Resistance 0.58: sustained litigation, women's-movement campaigns, and periodic uniform-code pushes meet the arrangement and are absorbed rather than defeated. Coordination type identity_coordination: the arrangement's primary function is boundary maintenance — deciding whose law governs whom. The FNL caution is taken seriously: identity framing ('this is who we are') is the arrangement's favorite cover story, and the coupling pattern here (extraction concentrated on powerless individuals at national scope) is exactly the signature the complexity offset does not excuse. Measurement series share one grid (t=0..30, step 5) across all three tracked metrics; trajectories are monotonic — extraction accumulation and enforcement intensification off a genuinely protective baseline — not oscillatory, so no cyclical commentary is required.
 *
 * PERSPECTIVAL GAP:
 *   From the minority-community seat the arrangement computes as coordination: it is the only structure standing between their family law and majority vote. From the interfaith-couple and women's seats the identical structure computes as enforced extraction: the same border-guarding that protects segments blocks their exits. The communal-authority seat experiences gatekeeping as stewardship; the legislature experiences paralysis as prudence; the court experiences case-by-case patching as fidelity to both pluralism and the constitution. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place minority_communities, majority_community_members, and communal_religious_authorities at the subsidy end (low d): the arrangement insures their family-law autonomy and, for the authorities, pays a jurisdictional monopoly. Victim declarations place interfaith_couples and women_under_personal_law at the target end (high d): they pay through the same structure that pays the beneficiaries, and their exits are constrained or identity-locked, pushing them toward the full-target pole. Two overrides correct derivations the structural data cannot supply: unaffiliated_individuals (powerless -> d 0.85) are excluded from the victim arrays by the arrangement's own community-unit design while bearing real, unrepresented costs; and a single institutional override (d 0.38) covers both institutional seats, whose canonical fallback would read them as symmetric administrators although both collect stability — the legislature buys electoral peace, the court preserves its adjudicative monopoly — placing them mildly on the beneficiary side of symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope keeps both halves visible. Reading the arrangement as pure rope (its self-description) would license ignoring the payers — the women and interfaith couples whose costs are dismissed as the price of peace. Reading it as pure snare would license demolition — abolishing the one architecture that shields minority family law from majoritarian rewrite, plausibly triggering the very domination the arrangement was built to prevent. The founding problem's status is contested, not dead: no mandatrophy is declared, and the R5 mismatch consumer should find no zombie signature (status contested x verdict world_rearranges). The temporal series nonetheless shows classic rent-layering: extraction and theater rising monotonically off a protective baseline, with the enforcement ratchet (suppression_requirement) climbing fastest — the bargain hardening around its beneficiaries as the founding threat evolves. Coalition note: the payer classes are individually powerless but structurally complementary (interfaith couples, gender-dissenting women, the unaffiliated fail the segmental order at different seams); a coalition is conceivable but impeded because each class's grievance is defined by a different community boundary, and identity locks keep most potential coalition members inside the segments whose authorities they would have to confront.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the marriage_authority kernel — the federalist_millet_reading. Where exactly do the five readings disagree, and what would each sibling change structurally?',
    'Comparative analysis of the sibling stories'' victim/beneficiary sets and epsilon values: secularist_reading relocates authority to the legislature (communal authorities become payers); gender_rights_reading recenters on intra-community equality (victim set expands, epsilon rises); communal_autonomy_reading removes the elite-bargain frame (epsilon drops toward pure coordination); judicial_harmonization_reading replaces legislative paralysis with case-by-case constitutional floors.',
    'Adopting any sibling''s premise rewrites this story''s beneficiary/victim structure and its epsilon; the disagreement is located in the unit of marriage authority (community versus person) and in whether plurality is terminal design or transitional anomaly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of five readings of the marriage_authority kernel; disagreement located in the unit of authority and the permanence of pluralism.').

omega_variable(
    protection_vs_elite_capture,
    'Is the arrangement''s persistence explained by genuine minority protection (coordination) or by communal-elite rent preservation (extraction riding the protection story)?',
    'Compare segments where gatekeeping weakened (internal reform succeeded, civil options opened) against segments where elites retained monopoly: if minority security held where gatekeeping fell, protection and gatekeeping are separable; if security collapsed, they are fused.',
    'If separable, the extraction component is removable without sacrificing the coordination function (classification trends toward rope); if fused, the arrangement drifts toward snare with minority protection as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_elite_capture, empirical, 'Whether minority protection and elite gatekeeping are structurally separable.').

omega_variable(
    paralysis_feature_or_bug,
    'Is legislative paralysis on family law a stability feature of the consociational bargain (this reading''s claim) or the mechanism that blocks reform and preserves extraction?',
    'Normative weighting plus outcome comparison: polities that broke paralysis (enacted civil marriage or unified codes) versus those that kept it — did minority security and individual welfare diverge as the two framings predict?',
    'If paralysis is a feature, the arrangement''s low-moderate epsilon stands and reform pressure should route through communities; if a bug, paralysis is an enforcement instrument and effective epsilon is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paralysis_feature_or_bug, preference, 'Valence of legislative paralysis: stability mechanism or reform blockade.').

omega_variable(
    majoritarian_threat_liveliness,
    'How live is the majoritarian-domination threat that constitutes the bargain''s protective justification?',
    'Track majority-political manifestos, legislative attempts at uniform-code introduction, and incidents of interference in minority family law across the interval; outside attestation from minority-rights monitors.',
    'A receding threat undermines the coordination justification and shifts weight to the extraction components; a resurgent threat strengthens the coordination reading and raises further the already prohibitive cost of any fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_threat_liveliness, empirical, 'Liveliness of the founding threat that anchors the protective framing.').

omega_variable(
    suppression_structural_vs_internalized,
    'Of the measured suppression keeping individuals inside communal jurisdiction, how much is structural (no routine civil route, notice regimes, conversion policing) and how much internalized (communal-duty framing, family enforcement)?',
    'Post-exit trajectory study of individuals who left their community: if perceived obligation persists after legal barriers are removed, the internalized share is substantial.',
    'Internalized suppression travels with the target after exit, raising effective suppression above the structural measure and complicating any purely legislative fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between legal architecture and internalized communal obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__federalist_millet_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__federalist_millet_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__federalist_millet_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(marr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_authority__federalist_millet_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority__federalist_millet_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t25, marriage_authority__federalist_millet_reading, base_extractiveness, 25, 0.33).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(marr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_authority__federalist_millet_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__federalist_millet_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority__federalist_millet_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__federalist_millet_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t25, marriage_authority__federalist_millet_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(marr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'religious family-law pluralism / personal law system' decomposes into five structurally distinct readings of one kernel (marriage_authority), per the epsilon-invariance principle: communal_autonomy_reading (tradition-grounded, epsilon near-zero), federalist_millet_reading (this file: elite-bargain anti-tyranny design, epsilon low-moderate), judicial_harmonization_reading (court-driven constitutional floor, epsilon moderate), gender_rights_reading (equality-centered, epsilon high), secularist_reading (transitional anomaly with terminal sunset, epsilon moderate). Each is a separate constraint with its own epsilon, beneficiaries, and victims; they are linked because they contest the same standing arrangement and cite one another. Upstream/downstream: communal_autonomy supplies the traditional material this bargain organizes; gender_rights and secularist supply the repudiation pressure recorded in this story's drift_state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerless, 0.85).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
