% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Sex/Gender Category Membership (Identity Reading)
 *   domain: social/legal
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   sex/gender category. Under the identity reading, category membership (who
 *   counts as 'woman' or 'man') is determined by subjective gender identity
 *   and self-identification, not by immutable reproductive biology. This
 *   reading benefits trans women by including them in the category and
 *   granting access to institutions organized around that category; it
 *   imposes costs on stakeholders who claim sex-based rights exclusive to
 *   reproductive categories and on those who frame sex categories as
 *   immutable. The constraint operates through institutional rule-setting,
 *   policy adoption, and cultural narrative enforcement. It is actively
 *   enforced in jurisdictions that adopt it, and it faces sustained
 *   resistance from biology-reading and gender-critical stakeholders. The
 *   measured extractiveness (0.68) reflects the asymmetric transfer of
 *   category-definition authority and access rights; the suppression (0.72)
 *   reflects the effort required to enforce identity-inclusive boundaries
 *   against alternative framings; the theater ratio (0.41) reflects the gap
 *   between the stated coordination function (recognizing trans identity) and
 *   the actual institutional work (managing contested space access and
 *   boundary disputes). The claim is tangled_rope: there is genuine
 *   coordination (institutional recognition of trans identity solves a real
 *   problem for trans people and institutions serving them), but the
 *   arrangement also extracts from cis women who lose exclusive control over
 *   sex-based categories. The measurement series tracks rising extractiveness
 *   and theater as the constraint moves from jurisdictional adoption phase to
 *   enforcement phase — as identity-based rules solidify institutionally, the
 *   cost of enforcing them against resistance rises, and the proportion of
 *   institutional work devoted to managing category disputes (theater)
 *   increases.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiaries under this reading; gain institutional recognition and access rights; identity_locked exit (cannot renounce identity without dissolving participation)
 *   - gender_affirming_legal_advocates: agenda-setters; define institutional rules and frame the problem; organized power; mobile exit (can shift strategies)
 *   - cis_women_sex_based_rights_claimants: primary payers; lose exclusive claim to 'woman' category and to sex-based protections; constrained exit (bounded between accepting expanded category, litigating, or abandoning sex-based framing)
 *   - gender_critical_stakeholders: secondary payers; institutional authority denied; constrained exit (can litigate or exit advocacy/academic positions but cannot change the reading from outside)
 *   - institutional_rule_bodies: co-agenda-setters; adopt and enforce identity-based rules; institutional power; face compliance burden and litigation risk from competing reading
 *   - biological_sciences_community: observers; produce empirical material cited by all readings; analytical position; provide data but do not determine institutional category boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Sex/Gender Category Membership (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, 'd19b6928-6f1e-437e-9bdf-b1dad1a62413').
narrative_ontology:cs_kernel_codification('d19b6928-6f1e-437e-9bdf-b1dad1a62413', formalized).
narrative_ontology:cs_authority_grounding('d19b6928-6f1e-437e-9bdf-b1dad1a62413', extraction).
narrative_ontology:cs_interpretation_layer_present('d19b6928-6f1e-437e-9bdf-b1dad1a62413').
narrative_ontology:cs_reading_relation('d19b6928-6f1e-437e-9bdf-b1dad1a62413', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('d19b6928-6f1e-437e-9bdf-b1dad1a62413', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d19b6928-6f1e-437e-9bdf-b1dad1a62413', foundational, gender_identity_category_sufficiency).
narrative_ontology:cs_axiom_status(gender_identity_category_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('d19b6928-6f1e-437e-9bdf-b1dad1a62413', gender_identity_category_sufficiency, deontological).
narrative_ontology:cs_axiom('d19b6928-6f1e-437e-9bdf-b1dad1a62413', foundational, self_determination_principle).
narrative_ontology:cs_axiom_status(self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('d19b6928-6f1e-437e-9bdf-b1dad1a62413', self_determination_principle, deontological).
narrative_ontology:cs_reference_frame('d19b6928-6f1e-437e-9bdf-b1dad1a62413', gender_identity_sufficiency_doctrine).
narrative_ontology:cs_drift_state('d19b6928-6f1e-437e-9bdf-b1dad1a62413', contemporary_institutional_adoption_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d19b6928-6f1e-437e-9bdf-b1dad1a62413', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_affirming_legal_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_sex_based_rights_claimants).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_stakeholders).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_identity_autonomy).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, trans women are included in the legal and social category 'woman' on the basis of their subjective gender identity. They gain access to women-only spaces (restrooms, shelters, sports categories), legal documentation reflecting their identity, and formal recognition as members of the protected class experiencing sex-based discrimination. Their exit from this framing is unavailable without fundamentally renouncing their identity — the constraint operates through the same mechanism (self-identification) that constitutes their participation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Legal scholars, advocacy organizations, and policy bodies that advocate for sex/gender categories determined by identity self-identification. They set institutional rules (Title IX interpretations, civil rights protections, institutional policies), litigate test cases, and frame the binding interpretation of what 'woman' and 'man' legally mean. They benefit from the constraint through institutional authority and resource flows directed to their advocacy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_affirming_legal_advocates, agenda_setter,
    organized, generational, mobile, global).

% Cis women who frame their interests through immutable sex-based characteristics and claim sex-specific legal protections (e.g., domestic violence shelters, women-only spaces, pregnancy accommodations). Under the identity reading, their exclusive hold on the 'woman' category is diluted by including trans women who may not share the biological characteristics that motivated sex-based protections. Their alternatives are accepting the expanded category, litigating for separate category systems, or abandoning sex-based framing entirely — all constrained and contested.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_sex_based_rights_claimants, payer,
    moderate, biographical, constrained, global).

% Individuals and organizations that hold the core axiom that sex categories must be based on immutable biological characteristics and that self-identification cannot override material reality for legal and social purposes. Under this reading, their framing is not institutionally endorsed, their category-definitions are overruled, and they bear the cost of institutional non-recognition. Many are displaced from positions in women's organizations and scholarship where they previously held authority.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_stakeholders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_critical_stakeholders, excluded).

% Courts, legislatures, civil rights agencies, and institutional governance bodies (universities, healthcare systems, sports authorities) that must interpret and enforce category membership rules. Under the identity reading, they adopt policies that recognize gender identity as sufficient for category membership. They bear the compliance cost of managing institutional spaces and rules around the expanded category, and they face litigation risk from parties claiming rights under the competing biology reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, institutional_rule_bodies, agenda_setter,
    institutional, generational, mobile, national).

% Their stake is indirect: the constraint establishes category-membership rules that do not directly affect them but influence resource allocation, institutional policy, and cultural meaning-making. They are positioned as observers rather than payers or beneficiaries, though some align politically with either the cis-women or gender-critical stakeholders.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_men, observer,
    moderate, biographical, mobile, global).

% Under the identity reading, trans men are included in the legal and social category 'man' by the same mechanism. They are structurally positioned similarly to trans women but face different material constraints (exclusion from women-only spaces they may need access to, such as domestic violence shelters). Their voice in the contest is often subsumed under the broader trans advocacy position, but their interests may diverge.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, excluded,
    moderate, biographical, identity_locked, global).

% Scientific researchers and biomedical communities that produce data on sex differentiation, hormones, chromosomes, and development. They provide empirical material cited by both the identity and biology readings. Their role is observational — they measure and describe; the constraint determines what categories those descriptions populate.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, biological_sciences_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, gender_affirming_legal_advocates).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and social category for persons whose lived gender identity and social role are female, regardless of reproductive biology. This solves the coordination problem of institutional recognition: what category of legal personhood encompasses trans women's lived social and psychological reality? The reading treats identity-based category membership as the solution to the problem of how to formally recognize gender transition.
% TRANSFER_FUNCTION: Moves access rights, institutional authority, and symbolic recognition from category-gatekeepers (those who previously defined 'woman' exclusively through biology) to trans women and identity-affirming advocates. Cis women lose exclusive control over the definition and lose some claim to sex-exclusive institutional spaces and legal protections. Gender-critical stakeholders lose institutional authority to define sex categories. The constraint transfers meaning-making power from biology-based framing to identity-based framing.
% ABSENT_VOICES: Gender-critical feminists and biological-sciences advocates are not seated at the institutional bodies that adopt the identity-reading policies in many jurisdictions; they attest the category definition has changed in their absence and that the resulting institutions do not reflect their interests. Trans men's distinct material interests (e.g., healthcare access, shelter access, sports eligibility) are often aggregated with trans women's interests rather than voiced separately.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight and sex/gender categories reverted to a biology-only basis, institutional spaces would reorganize (trans women excluded from women-only facilities, legal documents reissued), trans women would lose formal legal recognition, sex-based rights frameworks would narrow to immutable reproductive categories, and the social meaning of sex would shift. Conversely, if it remained and solidified, institutional infrastructure would continue to recognize identity-based membership, cis women's sex-based organizing would need to specify biological criteria explicitly, and the boundary of 'woman' would remain permeable to identity claims.
% FOUNDING_PROBLEM: What institutional framework recognizes the gender identity of trans persons as a valid basis for legal and social category membership? How should legal systems handle persons whose gender identity diverges from sex assignment at birth?
% FOUNDING_PROBLEM_CORROBORATION: Trans advocates and gender-affirming legal scholars attest the founding problem is live and urgent: trans people face institutional barriers to legal recognition and access to spaces and services matching their gender identity. Cis women's sex-based rights advocates attest the founding problem is a recent invention, not a longstanding institutional gap — they argue it is a competing normative claim (how to protect sex-based rights) being framed as a coordination problem. Biological-sciences observers attest the founding problem presumes a category boundary question (what 'woman' means institutionally) that science does not resolve.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 terminal) because the constraint redistributes category-definition authority and institutional space access from biology-reading stakeholders to identity-reading stakeholders. The asymmetry is structured: trans women gain access and recognition they lacked; cis women lose exclusive hold on sex-specific institutions and legal frameworks; gender-critical stakeholders lose institutional authority to define sex categories. This is not mere coordination cost — it is redistribution of a contested good (the meaning of 'woman'). The measurement series shows rising extractiveness over time (0.45 to 0.68 over interval 0–25) because institutional adoption is incremental: early in the adoption phase, identity-based rules coexist with biology-based rules in different jurisdictions, creating hybrid enforcement and lower measured extraction. As the reading consolidates and becomes dominant in institutional contexts, cis women's alternatives narrow and gender-critical stakeholders face more uniform non-recognition, raising effective extraction. Suppression is high (0.72) because enforcement of identity-inclusive boundaries requires active prevention of biology-based category gatekeeping. The constraint does not naturally persist through participant preference — cis women's sex-based rights advocates and gender-critical stakeholders have strong structural incentive to oppose it. Maintenance requires institutional policy backing, legal enforcement, and cultural narrative work (suppression of the biology reading as a viable institutional option). Theater ratio (0.41) reflects significant institutional work devoted to managing boundary disputes rather than to the original coordination function. Early in adoption, institutions frame identity-based policies as straightforward recognition ('we are including trans people'); over time, institutions must spend increasing effort on contested issues (space allocation, sports eligibility, healthcare protocols, documentation procedures) where identity-inclusive categorization creates new coordination problems. The theater series (0.22 to 0.41) shows this drift: as the reading becomes institutionalized, the proportion of enforcement activity that is performative boundary-management (managing external disputes, issuing clarifications, defending against legal challenges) rises relative to the original coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The identity reading produces sharply divergent per-seat classification from an institutional beneficiary seat vs. a payer seat. From the agenda-setter seat (gender-affirming advocates, institutional rule bodies), the constraint appears as genuine coordination: solving a real problem (recognizing trans identity) through cooperative institutional work. From this seat, extractiveness is modest and suppression is necessary coordination overhead. From the cis-women payer seat, the same structure appears as enforced extraction: category authority has been transferred without consent, institutional spaces previously available to women are now shared on criteria the payer did not choose, and alternatives have been suppressed (biology-based categorization is no longer institutionally viable in many contexts). From this seat, extractiveness is high and suppression feels like institutional coercion. The engine computes both per-seat classifications from the structural data — the stakeholder positions, exit options, power levels, and directionality. The authored claim (tangled_rope) is compatible with both readings: tangled rope by definition has genuine coordination AND asymmetric extraction; the per-seat computations reveal where each sits on the extraction/coordination spectrum. A snare reading (pure extraction, no genuine coordination) would be authored if the constraint served no function for trans people or institutions; this one does — the coordination is real. But the coordination is achieved through enforcement that constrains alternatives, and the beneficiaries and payers are asymmetric by structure, not by accident.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women (beneficiary, identity_locked) have d near the full-beneficiary end (low d, ~0.15–0.25): they gain access and recognition they lack, identity-inclusive categorization is the mechanism of their participation (they cannot exit without renouncing identity), their alternatives are severely constrained (biology-only categorization denies them institutional recognition). Their directionality is beneficiary despite the identity-lock exit option because the constraint directly subsidizes their institutional position. Gender-affirming advocates (agenda_setter, organized, mobile) have d near the full-beneficiary end (~0.20–0.30) because they set rules and gain institutional authority; they have mobile exit (can shift strategies or work in different jurisdictions) which moderates extraction directed at them. Cis women sex-based rights claimants (payer, constrained) have d near the full-target end (high d, ~0.75–0.85): they lose exclusive access to sex-specific categories and institutions, alternatives are constrained (can litigate but institutional direction is set), and they bear extraction through forced category-sharing. Gender-critical stakeholders (payer, constrained) have d similarly high (~0.75–0.85): institutional authority is denied, they cannot change the reading from within the institutions that have adopted it, alternatives are constrained. Institutional rule bodies (agenda_setter, institutional, mobile) have d near symmetric (~0.45–0.55) because they both implement the reading (collecting institutional authority) and bear compliance burden (managing disputes, facing litigation). A biology-reading institutional body would have very different directionality — in that context, cis women and gender-critical stakeholders would be beneficiaries and trans women would be payers. The difference illustrates the reading-dependence: directionality is not an observer-invariant fact; it depends on which reading's standing arrangement we are measuring.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to institutionally recognize trans gender identity) is live, and the founding coordination function (providing institutional recognition and access) is active. However, there is significant built-in conflict: the constraint achieves coordination for trans people by enforcing category expansion that extracts from cis women's sex-based rights frameworks. This is not mandatrophy in the strict sense (a constraint whose function has completely atrophied), but it is a constraint where the original founding problem has been expanded in scope beyond its initial framing. The reading declares the kernel problem as 'how to recognize trans identity institutionally'; it does not declare as a founding problem 'how to expand women's category boundaries in a way that preserves cis women's sex-based rights.' Because the constraint simultaneously solves one coordination problem and creates a different one (boundary conflict), there is structural instability. Mandatrophy is not (yet) present, but the constraint carries high risk of decay into either a snare (if the coordination function is gradually subordinated to pure category enforcement) or into a piton (if the coordination function is achieved and institutions maintain identity-inclusive rules for inertial reasons rather than because they solve trans-specific problems). The six-questions mismatch check (founding_problem_status=live + disappearance_verdict=world_rearranges) indicates the constraint is actively functioning, not zombie-like. But the conflict-ridden nature of that functioning should be monitored for mandatrophy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_boundary_fuzziness,
    'What degree of subjective certainty, duration, and behavioral consistency constitutes valid gender identity for purposes of category membership? Is identity binary or spectrally distributed?',
    'Institutional rules specifying criteria (e.g., diagnosis of gender dysphoria, duration of identity claim, legal documentation process) and empirical data on the stability and false-positive rate of identity-based self-identification. Jurisdictional variation provides quasi-experimental evidence.',
    'If identity criteria are permissive and low-cost, enforcement suppression may be lower than authored (boundary crossing is easier, so suppression of alternatives is less necessary). If criteria are restrictive (medical gatekeeping, documentation burdens), effective suppression rises and the constraint begins resembling the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_boundary_fuzziness, empirical, 'The operational definition and gatekeeping level of identity-based category membership.').

omega_variable(
    cis_women_material_interests_divergence,
    'Is the set of cis women''s material interests (sex-based violence protection, reproductive healthcare, sport eligibility) genuinely unified under a single ''woman'' category, or do they diverge across contexts?',
    'Empirical investigation of the specific physical/material conditions under which sex-based categorization is functionally relevant (e.g., domestic violence shelters do sex-based intake; sports categories do sex-based eligibility). For each, does identity-inclusive categorization degrade function, or is function independent of identity?',
    'If cis women''s material interests are context-dependent and identity-inclusive categorization preserves function in most contexts, the measured extractiveness may be overstated (less actual victim-side loss). If identity inclusion materially degrades function in core contexts, extractiveness is accurately measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_material_interests_divergence, empirical, 'Whether cis women''s sex-based protections degrade under identity-inclusive categorization.').

omega_variable(
    trans_women_material_equivalence,
    'For the specific institutional functions sex-based categorization serves (violence protection, healthcare, sport, etc.), are trans women materially equivalent to cis women, or do they differ in ways that affect function?',
    'Context-specific empirical investigation: domestic violence shelter intake (trauma histories, physical safety needs); healthcare (reproductive medicine, hormone-responsive conditions); sports (performance metrics, injury risk, fairness standards). Literature review and institutional outcome data.',
    'If trans women are materially equivalent across institutional functions, the identity reading fully resolves coordination; if they differ, the constraint may require context-specific sub-categorization, which contradicts the unified identity-based framing and increases effective suppression (enforcement cost to maintain unified boundaries rises).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trans_women_material_equivalence, empirical, 'Whether trans women and cis women are materially equivalent for sex-based institutional functions.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the identity reading logically foreclose the biology reading (making both untenable in a single framework), or do they merely coexist as competing normative claims?',
    'Philosophical analysis of the core axioms: identity reading asserts gender identity determines category membership; biology reading asserts immutable reproductive biology determines it. Do these premises contradict (one true => one false), or can both be held in different domains or by different parties?',
    'If they foreclose, the constraint instantiates genuine conflict that must be resolved by institutional dominance. If they coexist, institutional pluralism is theoretically possible (identity-based recognition in one domain, biology-based in another), and the measured suppression reflects the cost of enforcing exclusive adoption of one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the identity and biology readings are logically foreclosing or merely competing.').

omega_variable(
    institutional_gatekeeping_capture,
    'Are gender-affirming institutional bodies genuinely committed to identity-based categorization, or are they captured by advocacy movements that benefit from institutional authority? Are they gatekeeping by advocacy design or genuine coordination problem-solving?',
    'Institutional history: do the bodies adopt identity-based rules in response to genuine coordination problems (institutions asking ''how do we recognize trans people'') or in response to organized advocacy pressure? Do they allow for boundary-condition debate, or do they suppress it? Comparative institutional analysis across jurisdictions.',
    'If institutions are genuinely solving coordination problems, the beneficiary framing is accurate and extractiveness is modest. If institutions are captured by advocacy and enforcing an identity reading against competing readings without genuine problem-solving, extractiveness rises and the constraint resembles a snare with institutional gatekeepers as primary beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_capture, empirical, 'Whether institutional adoption of identity-based rules reflects genuine coordination or advocacy capture.').

omega_variable(
    reading_kernel_alternative_framings,
    'Are the identity, biology, and hybrid readings the only coherent framings of the sex/gender category kernel, or do other framings exist?',
    'Philosophical and anthropological analysis: are there historically documented or theoretically coherent alternatives (e.g., process-based readings where category membership depends on institutional recognition rather than identity or biology, or relational readings where membership is defined by social role)? If so, do they coexist with or foreclose the three standard readings?',
    'If alternatives exist and are suppressed, the framework is more constraining than the three-reading model suggests, and measured suppression may understate enforcement costs. If only three are coherent, the reading assignment is complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_alternative_framings, conceptual, 'Whether the identity/biology/hybrid taxonomy exhausts the coherent readings of the sex/gender category kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(sex__tr_t4, observed).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(sex__tr_t8, observed).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(sex__tr_t12, observed).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__identity_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(sex__tr_t16, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sex__tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(sex__be_t4, observed).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(sex__be_t8, observed).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(sex__be_t12, observed).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__identity_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(sex__be_t16, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__identity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sex__be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(sex__su_t4, observed).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(sex__su_t8, observed).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(sex__su_t12, observed).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(sex__su_t16, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__identity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(sex__su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three distinct constraints corresponding to the three live readings: identity_reading, biology_reading, and hybrid_reading. Each has its own ε, beneficiary/victim structure, and coordination type. They share a kernel but differ in what determines category membership. The identity_reading is authoritative in some jurisdictions and institutional contexts; the biology_reading and hybrid_reading are authoritative in others. This network link indicates structural interdependence: changes in institutional adoption of one reading affect the resource environment and institutional legitimacy for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
