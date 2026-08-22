% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Gender-Rights Reading of Marriage Authority: The Equality-Litigation Channel over Personal Law
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   Within religiously governed personal-law systems, marriage, divorce,
 *   maintenance, and inheritance rules are administered by communal
 *   authorities. Women inside those systems contest the harshest practices —
 *   instant unilateral divorce, denial of post-divorce maintenance, exclusion
 *   from marital property — through constitutional equality litigation. This
 *   story authors the gender-rights reading of the marriage_authority kernel:
 *   the standing arrangement it indexes is the persistent patriarchal
 *   practice PLUS the institutionalized courtroom channel that contests it
 *   case by case. Read by that reading's own lights, the channel delivers
 *   real but narrow, slow, and reversible relief, while the professional
 *   advocacy complex, communal leadership, the legislative class, and the
 *   judiciary each collect durable returns from the contest's continuation;
 *   the women whose grievances supply the docket remain largely stationary
 *   across generations. The claim (snare) and the metrics below are authored
 *   independently; a harmonization-flavored seat may compute a hybrid
 *   verdict, and that divergence is the datum the corpus exists to take. KEY
 *   AGENTS (by structural relationship): - womens_rights_advocacy_complex:
 *   Primary beneficiary (organized/mobile) — runs the litigation pipeline,
 *   collects precedent, funding, and standing - male_religious_authorities:
 *   Agenda-setter and incidental beneficiary (powerful/identity_locked) —
 *   administers communal marriage rules, harvests backlash mobilization -
 *   supreme_court_judiciary: Agenda-setter of the channel
 *   (institutional/constrained) — sets the doctrinal pace -
 *   women_within_patriarchal_personal_law: Primary target (powerless/trapped)
 *   — bears the subordination and the costs of contest -
 *   national_legislative_class: Secondary beneficiary (institutional/mobile)
 *   — harvests the electoral conflict, blocks the comprehensive fix -
 *   internal_communal_reformers: Excluded voice (moderate/identity_locked) —
 *   within-tradition reform crowded out - international_human_rights_bodies:
 *   Analytical observer (institutional/analytical)
 *
 * KEY AGENTS:
 *   - womens_rights_advocacy_complex: Primary beneficiary (organized/mobile) — operates the case pipeline; funding, staffing, and public standing scale with docket volume and visibility
 *   - male_religious_authorities: Agenda-setter with incidental beneficiary position (powerful/identity_locked) — sets the communal rules under challenge and converts adverse rulings into mobilizational capital
 *   - supreme_court_judiciary: Agenda-setter of the channel itself (institutional/constrained) — controls doctrinal pace; collects docket significance and prestige
 *   - women_within_patriarchal_personal_law: Primary target (powerless/trapped) — lives under rules she did not author; pays in subordination, exposure, and years; receives occasional narrow relief
 *   - national_legislative_class: Secondary beneficiary (institutional/mobile) — monetizes the recurring conflict electorally while avoiding comprehensive reform
 *   - internal_communal_reformers: Excluded (moderate/identity_locked) — pursues within-tradition change that the courtroom pipeline starves of attention
 *   - international_human_rights_bodies: Observer (institutional/analytical) — documents the gap between guarantee and practice; no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.72).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.68).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Gender-Rights Reading of Marriage Authority: The Equality-Litigation Channel over Personal Law").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '8a7b25dc-21dc-4348-a476-5c75e21ea92c').
narrative_ontology:cs_kernel_codification('8a7b25dc-21dc-4348-a476-5c75e21ea92c', fixed_text).
narrative_ontology:cs_authority_grounding('8a7b25dc-21dc-4348-a476-5c75e21ea92c', lineage).
narrative_ontology:cs_interpretation_layer_present('8a7b25dc-21dc-4348-a476-5c75e21ea92c').
narrative_ontology:cs_reading_relation('8a7b25dc-21dc-4348-a476-5c75e21ea92c', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('8a7b25dc-21dc-4348-a476-5c75e21ea92c', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a7b25dc-21dc-4348-a476-5c75e21ea92c', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('8a7b25dc-21dc-4348-a476-5c75e21ea92c', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('8a7b25dc-21dc-4348-a476-5c75e21ea92c', foundational, personal_law_subordinate_to_fundamental_rights).
narrative_ontology:cs_axiom_status(personal_law_subordinate_to_fundamental_rights, holdable).
narrative_ontology:cs_axiom_grounding('8a7b25dc-21dc-4348-a476-5c75e21ea92c', personal_law_subordinate_to_fundamental_rights, deontological).
narrative_ontology:cs_axiom('8a7b25dc-21dc-4348-a476-5c75e21ea92c', secondary, intra_community_equality_justiciable).
narrative_ontology:cs_axiom_status(intra_community_equality_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('8a7b25dc-21dc-4348-a476-5c75e21ea92c', intra_community_equality_justiciable, conventional).
narrative_ontology:cs_reference_frame('8a7b25dc-21dc-4348-a476-5c75e21ea92c', constitutional_equality_supremacy).
narrative_ontology:cs_drift_state('8a7b25dc-21dc-4348-a476-5c75e21ea92c', post_instant_divorce_ruling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a7b25dc-21dc-4348-a476-5c75e21ea92c', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, womens_rights_advocacy_complex).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, national_legislative_class).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, national_legislative_class).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, intra_community_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the litigation pipeline: identifies contestable practices, recruits plaintiffs, argues appeals, and converts rulings into doctrine and campaigns. Funding, staffing, and public standing scale with the volume and visibility of cases; organizations can pivot to other portfolios if the docket dries up. Individual clients come and go; the institutions persist across generations.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, womens_rights_advocacy_complex, beneficiary,
    organized, generational, mobile, national).

% Administer marriage, divorce, and inheritance within their communities through personal-law institutions and informal councils. They set the rules the courts are asked to strike down, and they mobilize followers against rulings they oppose — mobilization that has repeatedly returned them to the center of national politics after adverse decisions. Their standing is constituted by custodianship of tradition; abandoning that role would dissolve the position itself.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_religious_authorities, agenda_setter,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, male_religious_authorities, beneficiary).

% Hears the challenges, sets the pace of doctrinal change, and decides how far equality guarantees reach into family law. Gains docket significance and institutional prestige from the stream of landmark cases; remains bound by precedent, separation-of-powers limits, and the knowledge that the legislature can reverse its rulings, as happened after the 1985 maintenance decision.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Live under marriage rules they did not author: unilateral divorce, contested maintenance, unequal property. Those who litigate become public figures in their communities, face ostracism and retaliation, and wait years for narrow, sometimes reversed, relief. Leaving the community means losing children, property, and social world; staying means living under the rules. Occasional wins — the end of instant divorce, enforceable maintenance — accrue to them too, years after filing.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, beneficiary).

% Harvests the recurring conflict for electoral mobilization — defending or attacking personal law depending on constituency — while avoiding comprehensive reform whose costs land immediately and whose benefits diffuse. Pays governance costs each time a ruling forces a legislative response.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, national_legislative_class, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, national_legislative_class, payer).

% Work for change from inside the tradition — scriptural argument, community education, internal councils. Their methods are slower and receive little courtroom attention; the adversarial pipeline crowds out their approach, and their standing inside the community suffers when outsiders litigate on behalf of the community's women.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, internal_communal_reformers, excluded,
    moderate, generational, identity_locked, national).

% Review state compliance with equality treaties, record the gap between constitutional guarantees and family-law practice in concluding observations, and supply comparative material that courts and advocates cite. Hold no enforcement power of their own.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, womens_rights_advocacy_complex).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals inside closed communal legal orders a state-backed, precedent-generating path to contest marriage practices, converting isolated grievances into enforceable doctrine without requiring legislative consensus or communal consent.
% TRANSFER_FUNCTION: Moves grievances, visibility, and years of litigants' lives upward into the court system; moves funding, precedent, careers, and standing into the advocacy-professional complex; moves mobilizational capital to communal leadership after adverse rulings; moves electoral issues to the legislative class. The underlying marriage rules themselves change slowly and partially.
% ABSENT_VOICES: Women who would prefer swift, quiet settlement inside community forums; internal communal reformers whose within-tradition methods the courtroom pipeline crowds out; and households bearing the collateral costs of criminalized practices, whose lost income lands on the same women the reforms protect. They sit outside the courtroom coalition — in community councils, within-tradition study circles, and households — and enter the record mainly as respondents or backdrop.
% DISAPPEARANCE_RATIONALE: If the channel vanished overnight, the advocacy sector would lose its docket, funding base, and doctrinal archive; communal leadership would lose its principal mobilization trigger; the legislative class would lose a recurring electoral axis; the bench would lose a signature docket; pending litigants would be stranded; and the limited protections already won — maintenance enforcement, the prohibition on instant divorce — would lose their doctrinal guardian. The surrounding institutional ecology is organized around the contest's continuation.
% FOUNDING_PROBLEM: Women inside religiously governed personal-law systems had no state-accessible remedy against unilateral divorce, denial of post-divorce maintenance, and exclusion from marital property: communal forums answered to male authority, and legislatures declined to touch communal law for fear of backlash.
% FOUNDING_PROBLEM_CORROBORATION: Internal communal reform organizations, working from within the tradition and outside the advocacy complex, continue to document maintenance and property gaps; international treaty-body concluding observations record the same shortfalls; national household-survey data on women's asset ownership and divorce outcomes corroborate the persistence. Attestation does not rest on the benefiting parties alone.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because, on the reading's own lights, the arrangement's material product flows past its ostensible beneficiaries: subordination persists across generations, gains are narrow and reversible (the 1985 maintenance victory was legislatively reversed within a year; the 2017 instant-divorce ruling was followed by criminalization whose enforcement costs landed on the same households), and the durable surpluses — precedent, funding, careers, mobilizational capital, electoral issues, docket prestige — accrue to four other seats. Suppression (0.68) is a raw, unscaled structural property: exit is trapped (leaving the community forfeits children, property, and social world), and alternatives are crowded out rather than merely unavailable — internal reform is framed as bad faith, comprehensive legislation as majoritarian overreach, quiet settlement as betrayal. Accessibility collapse is moderate (0.55): the alternatives remain visible and occasionally used, but the channel discredits them systematically. Resistance is high (0.70): the record shows legislative reversal and mass communal mobilization meeting nearly every major advance. Theater ratio (0.45) reflects a maturing performance layer — repeated commissions restating decade-old findings, anniversary jurisprudence, hearing cycles — alongside genuine delivery. All three series run on one shared grid (T=0..40, anchored roughly 1985-2025: Shah Bano through the post-instant-divorce era); base_extractiveness and theater_ratio rise monotonically (rent-layering and proxy substitution), and suppression_requirement rises because the narrative specifically tracks the channel's enforcement capacity hardening — it increasingly polices its own monopoly on reform. No cyclical dynamics are asserted. Coalition note: the primary targets are individually powerless and rarely coordinate across communal lines; cross-community coalitions exist but are thin, which is itself a product of the identity-lock documented below. Claim and metrics are independent: a seat computing from the harmonization flank may return a hybrid type; that divergence is data, not error.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience structurally different arrangements. From the target seat (women within the communities), the arrangement is a trap with two mouths: the communal rules subordinate, and the contest consumes — years of exposure, community retaliation, and relief that arrives late, narrow, or reversed. From the advocacy seat, the same structure is an emancipation vehicle: each ruling is a brick in a rights architecture, and the docket is proof of mission. From the communal seat, it is external siege that validates defensive mobilization — every adverse ruling replenishes the very authority it attacks. From the bench, it is incremental constitutional justice, patient and legitimate. Identity-lock mechanics: the targets' exit is locked relationally and communally — self-concept, children, belonging, and meaning are constituted inside the community, so formal exit options are unusable; the authorities' lock is custodial — their position IS custodianship of tradition, so conceding the equality premise dissolves the office itself. If the targets' identity frame broke (mass cross-communal coalition or exit), the extraction base would evaporate and the entire structure would recompose; the classification therefore turns on a lock, not a barrier alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the advocacy complex and the legislative class near the beneficiary pole (low d): they collect the channel's product without bearing its costs. The victim declaration places women within patriarchal personal law near the full-target pole (high d), amplified by their trapped exit and the national dispersal of the practices, which makes verification of redress slow. Male religious authorities are the one seat the automatic derivation would misread: their beneficiary declaration reflects episodic backlash windfalls, not transfer receipt — their primary posture toward the channel is adversarial administration — so an override sets their d to 0.45 (mid-range), and the 'powerful' atom is unique to them so the override collides with no other seat. The judiciary declares no beneficiary/victim position; its directionality falls to the engine's institutional fallback, which is appropriate — its collection is prestige and docket significance, not the channel's material product. Observers sit at the analytical pole and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (maintenance and property gaps persist; new practices surface as old ones fall), and the world rearranges if the channel vanishes — so no obsolescence flag fires, and the mandate has not outlived its function. The classification work this story performs is preventive, in two directions. First, it stops the channel being mislabeled as pure coordination: the emancipation story is real but partial, and the receipt surface shows the durable gains landing in professional, communal, electoral, and judicial seats rather than in the target class. Second — and this is what the kernel decomposition buys — it keeps this channel's capture distinct from the underlying patriarchy's extraction, which is a DIFFERENT constraint that a communal-autonomy-reading story would author against the same referent under different lights. Collapsing the two would let each structure hide behind the other: the channel would cite the patriarchy to prove its necessity, and the patriarchy would cite the channel's slow pace to prove reform impossible. Two stories, linked by the network, deny both covers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexing,
    'This story indexes epsilon to the standing arrangement (persistent patriarchal personal-law practice plus the managed courtroom contest) under the gender-rights reading''s own lights; how would beneficiaries, victims, and computed type change if the marriage_authority kernel were read by a sibling reading instead?',
    'Author the four sibling stories (communal_autonomy_reading, secularist_reading, federalist_millet_reading, judicial_harmonization_reading), each indexing epsilon to the same standing arrangement under its own lights, and compare computed types across the family.',
    'Communal-autonomy lights would relocate benefit to the community and harm to state intrusion; secularist lights would locate the defect in pluralism itself; millet lights would locate it in majoritarian domination. Same referent, different epsilon and type per reading — cross-reading comparisons without re-indexing are invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Reading-indexed epsilon over a shared kernel referent; committer structure routed here rather than into standard fields.').

omega_variable(
    net_material_effect_on_litigating_women,
    'Does passage through the litigation channel leave litigating women materially better off than matched women who resolve inside community forums or never contest at all?',
    'Longitudinal cohort comparison of litigants versus matched non-litigants on income, housing, custody, safety, and community standing.',
    'A net-positive result supports a hybrid coordination/extraction verdict; a net-negative result confirms the channel as net extraction from the very class it claims to serve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_material_effect_on_litigating_women, empirical, 'Whether the channel''s material returns to women exceed the costs the channel itself imposes on them.').

omega_variable(
    backlash_compensation_pattern,
    'Do judicial victories systematically trigger compensatory communal-legislative hardening (the 1985-1986 maintenance-reversal pattern) that leaves the class position of women worse than before the ruling?',
    'Event-study analysis of major rulings followed by legislative reversals or communal counter-mobilization, measuring subsequent practice rather than doctrine.',
    'A robust compensation pattern would show the channel amplifying the subordination it contests, strengthening the pure-extraction reading of its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_compensation_pattern, empirical, 'Whether the channel''s wins are clawed back at the class level by backlash dynamics.').

omega_variable(
    suppressed_alternative_performance,
    'Would comprehensive legislative codification or within-tradition reform have delivered faster, deeper improvement absent the courtroom channel''s crowding-out of both?',
    'Comparative-jurisdiction analysis: codified-family-law states and internal-reform movements versus litigation-first jurisdictions, controlling for development level and religiosity.',
    'Demonstrated outperformance by suppressed alternatives converts the channel''s gatekeeping from side-effect into rent defense, raising effective suppression and hardening the extraction verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppressed_alternative_performance, conceptual, 'Counterfactual performance of the alternatives the channel marginalizes.').

omega_variable(
    advocacy_incentive_divergence,
    'How far do advocacy organizations'' revenue and standing incentives diverge from their clients'' material outcomes?',
    'Audit of funding cycles, staffing growth, and campaign strategies against outcome audits for the clients actually served.',
    'High divergence would justify raising the advocacy seat''s directionality above its derived beneficiary-low value and sharpen the capture finding on the receipt surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_incentive_divergence, empirical, 'Degree of incentive divergence between the professional complex and its clientele.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression operating on women within patriarchal personal-law communities structural (jurisdictional reach of communal forums, economic dependency, custody exposure) or internalized (duty-belief and identity fusion that would persist after barriers lift)?',
    'Post-exit trajectory study of women who leave their communities: if felt obligation and return-to-forum behavior persist after formal barriers are removed, reclassify the suppression as partially internalized.',
    'An internalized share raises effective suppression beyond the structural measure and predicts relapse into community forums even after exit options formally open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized composition of the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gender_rights_reading_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t0, observed).
narrative_ontology:measurement(gender_rights_reading_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t8, observed).
narrative_ontology:measurement(gender_rights_reading_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t16, observed).
narrative_ontology:measurement(gender_rights_reading_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t24, observed).
narrative_ontology:measurement(gender_rights_reading_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t32, observed).
narrative_ontology:measurement(gender_rights_reading_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(gender_rights_reading_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gender_rights_reading_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(gender_rights_reading_be_t0, observed).
narrative_ontology:measurement(gender_rights_reading_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(gender_rights_reading_be_t8, observed).
narrative_ontology:measurement(gender_rights_reading_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(gender_rights_reading_be_t16, observed).
narrative_ontology:measurement(gender_rights_reading_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(gender_rights_reading_be_t24, observed).
narrative_ontology:measurement(gender_rights_reading_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement_basis(gender_rights_reading_be_t32, observed).
narrative_ontology:measurement(gender_rights_reading_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(gender_rights_reading_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gender_rights_reading_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(gender_rights_reading_su_t0, observed).
narrative_ontology:measurement(gender_rights_reading_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(gender_rights_reading_su_t8, observed).
narrative_ontology:measurement(gender_rights_reading_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(gender_rights_reading_su_t16, observed).
narrative_ontology:measurement(gender_rights_reading_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(gender_rights_reading_su_t24, observed).
narrative_ontology:measurement(gender_rights_reading_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement_basis(gender_rights_reading_su_t32, observed).
narrative_ontology:measurement(gender_rights_reading_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(gender_rights_reading_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'reforming personal law' decomposes into five structurally distinct readings of the marriage_authority kernel, per the epsilon-invariance principle: each reading fixes a different legitimate holder of marriage authority and hence a different constraint with its own epsilon, beneficiary structure, and type. This file authors the gender-rights reading alone. Family edges run through affects_constraints; the upstream/downstream asymmetry is doctrinal — this reading's equality axiom supplies the substantive pressure that the harmonization reading channels procedurally, and each of its rulings shifts the baseline from which the secularist and millet debates start.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
