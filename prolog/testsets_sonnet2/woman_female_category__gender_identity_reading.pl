% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Category Membership by Self-Identification (Gender Identity Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the gender_identity_reading of the contested
 *   woman_female_category kernel: the claim that category membership in
 *   'woman'/'female' for legal, social, and institutional purposes is
 *   determined by internal self-identification, independent of chromosomal
 *   sex, reproductive anatomy, or developmental biology. This is one of three
 *   structurally distinct readings of the same kernel (the others are
 *   sex_biology_reading and hybrid_contextual_reading, generated as separate
 *   constraint stories). Under the ε-invariance principle, each reading gets
 *   its own ε, its own beneficiary/victim set, and its own classification —
 *   this file does not average across readings or describe the contest
 *   itself; it describes the standing arrangement as advanced and applied
 *   under this reading's own terms. The founding problem this reading answers
 *   — historical denial of legal recognition and surgical/sterilization
 *   prerequisites for reclassification — is real and independently
 *   corroborated; the dispute charted here is over whether
 *   self-identification as sole criterion is the right-sized remedy, given
 *   the costs it relocates onto single-sex-space occupants, competitive-sport
 *   participants, and sex-disaggregated data collection.
 *
 * KEY AGENTS:
 *   - transgender_women_seeking_legal_recognition: primary beneficiary of the standard (moderate/identity_locked) — gains recognition and access without biological gatekeeping
 *   - transgender_advocacy_organizations: agenda-setter (organized/mobile) — sets and defends the standard through litigation and model policy
 *   - natal_women_in_single_sex_spaces: primary payer (powerless/constrained) — loses biology-conditioned access to shelters, changing rooms, crisis services
 *   - female_athletes_in_competitive_sport: payer (moderate/constrained) — competitive category redefined without their negotiated consent
 *   - detained_natal_women_in_carceral_settings: most severely trapped payer (powerless/trapped) — no exit from shared housing outcomes
 *   - employers_seeking_liability_shielding_via_policy_uniformity: secondary beneficiary (institutional/arbitrage) — adopts the standard for administrative simplicity regardless of downstream effect on other seats
 *   - courts_and_legislatures: analytical observer (institutional/analytical) — adjudicates between this reading and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.52).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Category Membership by Self-Identification (Gender Identity Reading)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'eb198d99-8690-4c71-86fa-9f8b2b7083d7').
narrative_ontology:cs_kernel_codification('eb198d99-8690-4c71-86fa-9f8b2b7083d7', distributed).
narrative_ontology:cs_authority_grounding('eb198d99-8690-4c71-86fa-9f8b2b7083d7', distributed).
narrative_ontology:cs_reading_relation('eb198d99-8690-4c71-86fa-9f8b2b7083d7', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('eb198d99-8690-4c71-86fa-9f8b2b7083d7', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('eb198d99-8690-4c71-86fa-9f8b2b7083d7', foundational, self_attestation_is_dispositive_of_category_membership).
narrative_ontology:cs_axiom_status(self_attestation_is_dispositive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('eb198d99-8690-4c71-86fa-9f8b2b7083d7', self_attestation_is_dispositive_of_category_membership, deontological).
narrative_ontology:cs_axiom('eb198d99-8690-4c71-86fa-9f8b2b7083d7', secondary, dignity_recognition_requires_unconditional_category_access).
narrative_ontology:cs_axiom_status(dignity_recognition_requires_unconditional_category_access, holdable).
narrative_ontology:cs_axiom_grounding('eb198d99-8690-4c71-86fa-9f8b2b7083d7', dignity_recognition_requires_unconditional_category_access, deontological).
narrative_ontology:cs_reference_frame('eb198d99-8690-4c71-86fa-9f8b2b7083d7', identity_self_attestation_as_dispositive).
narrative_ontology:cs_drift_state('eb198d99-8690-4c71-86fa-9f8b2b7083d7', post_2015_legal_recognition_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('eb198d99-8690-4c71-86fa-9f8b2b7083d7', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women_seeking_legal_recognition).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_advocacy_organizations).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, employers_seeking_liability_shielding_via_policy_uniformity).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, female_athletes_in_competitive_sport).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, sex_based_data_collection_efforts).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, detained_natal_women_in_carceral_settings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition as women consistent with their gender identity, and access to female-designated spaces, services, and protections without requiring proof of medical transition. Under this reading, their self-identification is dispositive of category membership; withholding recognition is experienced as an assault on dignity and personhood. Their exit from the identity itself is not available — it is who they are — but their ability to secure recognition varies enormously by jurisdiction and institution.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women_seeking_legal_recognition, beneficiary,
    moderate, biographical, identity_locked, national).

% Litigate, lobby, and set institutional policy language to establish self-identification as the operative legal and administrative standard for sex/gender category membership. They administer the framework's spread through model legislation, corporate policy templates, and professional association guidance, and could revise the framework's scope (e.g., context-limited carve-outs) but generally resist doing so as a matter of principle.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, transgender_advocacy_organizations, beneficiary).

% Use shelters, changing rooms, prisons, rape crisis centers, and other single-sex spaces premised on shared biological vulnerability or trauma history. Under a self-identification standard, category membership in these spaces is no longer conditioned on biological sex, which some experience as a loss of a space they relied on. Their exit option is to avoid the institution altogether — often not a real option for a shelter or a prison — which is a constrained, not a mobile, exit.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces, payer,
    powerless, biographical, constrained, local).

% Compete in female divisions premised on the average physiological differences between male and female puberty-developed bodies. Under self-identification, category membership in the female division follows declared gender rather than developmental biology, which competitors argue changes competitive outcomes. Their exit option is to leave the sport or the division, forfeiting years of training investment — a high-cost, not a free, exit.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, female_athletes_in_competitive_sport, payer,
    moderate, biographical, constrained, national).

% Medical research, epidemiology, and public health surveillance that depends on recording biological sex as a variable (e.g., drug dosage response, maternal health outcomes, sex-linked disease risk). Under a self-identification standard applied uniformly across administrative recordkeeping, biological sex data becomes harder to collect cleanly, degrading the evidentiary base these efforts depend on. Listed for completeness as a non-agent structural interest, not a party with standing.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_based_data_collection_efforts, payer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(woman_female_category__gender_identity_reading, sex_based_data_collection_efforts).

% Incarcerated women housed in facilities that, under self-identification policy, may house self-identified trans women convicted of violent or sexual offenses in the same units. They have no capacity to leave the facility, choose their housing unit, or refuse cohabitation; this is the most trapped exit position in the constraint's operation.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, detained_natal_women_in_carceral_settings, payer,
    powerless, immediate, trapped, regional).

% Adopt self-identification as institutional policy for restrooms, dress codes, and HR recordkeeping because it is legally simpler to administer a single non-adjudicable standard than to evaluate individual claims or biological status. This reduces litigation exposure and administrative burden for the institution regardless of how it affects the parties who must share the resulting spaces.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, employers_seeking_liability_shielding_via_policy_uniformity, beneficiary,
    institutional, biographical, arbitrage, national).

% Adjudicate disputes between the competing readings of the kernel — whether self-identification, biological sex, or a context-dependent hybrid governs category membership for a given legal purpose. They hear evidence and argument from all sides and can issue rulings or statutes that shift which reading has legal force in a given domain.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple, non-adjudicable rule for who counts as a woman for legal, social, and institutional purposes — avoiding the need for institutions to investigate or gatekeep individuals' bodies, medical histories, or documentation, and extending dignity-based recognition to people whose lived identity does not match natal sex.
% TRANSFER_FUNCTION: Moves the burden of category-boundary uncertainty from the individual claiming membership (who no longer must prove medical transition or biological status) to natal-sex-based institutions and their existing occupants (who lose the ability to condition access on biological sex, and absorb the resulting safety, privacy, competitive-fairness, or data-integrity costs).
% ABSENT_VOICES: Detained natal women, domestic violence shelter residents, and rape crisis survivors are rarely direct parties to the litigation and legislative processes that set this standard — their objections surface mainly through amicus filings, journalism, and after-the-fact case reports, not as seated negotiators when the policy is set. Intersex individuals whose biology does not map cleanly onto either category are also largely absent from this reading's framing, which treats the binary itself as unproblematic and only relocates its gatekeeping criterion.
% DISAPPEARANCE_RATIONALE: If self-identification as the operative standard vanished overnight and biological sex reverted to controlling in every domain, legal recognition, documentation, sport eligibility, and space-access rules for transgender individuals would revert to pre-transition status in many jurisdictions; conversely, if the biology-based reading vanished, single-sex space administration and sport categorization would need to be rebuilt on a different axis. The world under contest here genuinely depends on which reading holds — this is not decorative disagreement.
% FOUNDING_PROBLEM: Transgender individuals faced systemic denial of legal recognition, exclusion from spaces and services consistent with their lived identity, and requirements (in many jurisdictions historically) to undergo surgery or sterilization before any legal recognition was available at all — a genuine dignity and access problem with documented harms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by clinical and legal literature on historical surgical/sterilization prerequisites for legal sex reclassification (documented in multiple national court rulings prior to reform), and by human rights bodies outside the advocacy organizations themselves. The contested question is not whether the founding problem was real, but whether self-identification-as-sole-criterion is the only or best remedy, versus a context-limited hybrid — that dispute is corroborated on both sides by legal scholars, sports governing bodies, and shelter-sector practitioners who are not party to either advocacy coalition.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is authored at a substantial-but-not-extreme level: this reading genuinely solves a real recognition problem for transgender individuals (the coordination function is not cover), but it does so by reallocating the biological-sex criterion that other parties relied on for safety, competitive fairness, or data integrity, without those parties' negotiated consent — that reallocation is the extractive component. Suppression (0.52) reflects that the standard is actively enforced through litigation, employment policy, and institutional compliance mandates once adopted, and that dissent from it (e.g., insisting on biology-based criteria in a given context) is frequently treated as actionable discrimination rather than a legitimate competing claim — this is real suppression of the sibling readings' operation in a domain, not merely disagreement. Resistance is high (0.72) because the standard meets sustained organized opposition from women's-sector organizations, some sport governing bodies, and legislatures adopting the hybrid reading instead. Theater ratio is moderate-low (0.28) and rising: as institutional adoption becomes more common, a growing share of compliance activity (diversity trainings, policy language, symbolic statements) is more performative than functionally load-bearing for the underlying recognition problem.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, this is straightforwardly a rope: a simple, humane, administratively workable rule that ends unnecessary gatekeeping. From the payer seats — especially detained natal women and shelter residents — the same rule computes as extractive, because it removes a criterion they relied on without offering them a comparable form of consent or negotiated accommodation. The engine should compute these as genuinely different per-seat classifications from the same structural data, not as a single averaged verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women seeking recognition are the structural beneficiary of this specific reading — the constraint, as authored, subsidizes their claim by making self-identification dispositive without requiring proof. Natal women in single-sex spaces, female athletes, and detained natal women are the payers: the standard as applied removes a criterion (biological sex) their access, safety, or competitive category previously depended on, and their exit options range from constrained (avoid the institution) to fully trapped (incarcerated women cannot choose their housing). Advocacy organizations are agenda-setters with mobile exit (they can relocate campaigns across jurisdictions) rather than beneficiaries in the direct-transfer sense — they administer and defend the standard rather than personally receiving from it. Employers are a secondary beneficiary class whose interest (liability reduction via administrative simplicity) is orthogonal to the dignity-recognition rationale but rides on the same policy language.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legal non-recognition and surgical/sterilization prerequisites — is corroborated as historically real and, per this reading's own account, still live in many jurisdictions (founding_problem_status: live). This blocks a mandatrophy read that would dismiss the entire arrangement as pure capture: the coordination function is genuine, not fabricated cover. What keeps this reading from resolving to a clean rope is the requires_active_enforcement flag and the named victim set with constrained-to-trapped exit — the standard's operation extends past resolving the founding problem into displacing a competing, also-legitimate criterion (biological sex) in contexts where that competing criterion served an independent, non-animus-based function (shelter safety, competitive categorization, carceral housing). Tangled Rope is the structurally honest claim: real coordination (dignity recognition) plus asymmetric extraction (relocated costs onto non-consenting payers) held together by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Should legal and administrative category membership for ''woman''/''female'' be governed by self-identification universally, by biology universally, or contextually by domain — and who has standing to decide which reading applies in which context?',
    'Not empirically resolvable in the ordinary sense: this is a genealogical/normative dispute about which criterion an institution''s underlying purpose (safety, fairness, dignity, recognition) actually requires in each domain. Legislative and judicial processes are attempting resolution jurisdiction-by-jurisdiction and context-by-context (the hybrid_contextual_reading is itself an attempted resolution mechanism).',
    'If courts and legislatures converge on the hybrid reading, this constraint''s scope narrows sharply (self-identification would govern only social/legal recognition, not sport/safety/medical contexts) and its victim set (athletes, shelter residents, detained women) would substantially shrink. If they converge on this reading universally, the victim set as authored here persists and likely deepens with scope. If they converge on the biology reading, this constraint effectively ceases to have institutional force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Which of the three kernel readings should hold institutional/legal force, and in which domains — the central live contest this story is one reading of.').

omega_variable(
    dignity_harm_vs_safety_harm_commensurability,
    'Are dignity/recognition harms to transgender individuals (from non-recognition) and safety/fairness/privacy harms to natal-sex-based space occupants and athletes (from the self-identification standard) commensurable on a single extraction scale, or are they qualitatively different kinds of harm that a single epsilon value cannot honestly aggregate?',
    'No empirical resolution mechanism exists for this — it is a foundational question about how to weigh different harm categories against each other, which different ethical frameworks answer differently. The epsilon value authored here (0.58) is this story''s own attempt at a single aggregate figure, adopted for the standing-arrangement referent per this reading''s own lights, not a claim that the underlying harms are actually commensurable.',
    'If the harms are treated as incommensurable, no single epsilon can honestly represent this constraint''s extraction, and the framework''s single-scalar model itself becomes a source of measurement distortion for this kernel specifically — this is a limit case for the epsilon-invariance principle applied to genuinely value-pluralistic disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_harm_vs_safety_harm_commensurability, conceptual, 'Whether recognition harms and safety/fairness harms can be aggregated onto one extraction scale at all.').

omega_variable(
    sample_size_and_context_dependence_of_safety_claims,
    'How frequently, in practice, does self-identification-based access to single-sex spaces correlate with the safety, privacy, or competitive-fairness harms victims in this story describe, versus how often is the harm hypothetical or statistically rare relative to the recognition benefit delivered to the much larger beneficiary population?',
    'Empirical: incident data from shelters, carceral facilities, and sport governing bodies under varying policy regimes, compared across jurisdictions with different rules, controlling for reporting and selection effects.',
    'If harms are empirically rare and recognition benefits are empirically large and widespread, the authored extractiveness (0.58) would be too high relative to the standard''s actual operation. If harms are more frequent or severe than commonly reported (e.g., due to underreporting in carceral or shelter settings), the authored extractiveness could be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sample_size_and_context_dependence_of_safety_claims, empirical, 'The actual empirical frequency and severity of the harms attributed to this reading''s operation, versus the benefit it delivers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__gender_identity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__gender_identity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__gender_identity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_female_category__gender_identity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_female_category__gender_identity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_female_category__gender_identity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_female_category__gender_identity_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_female_category__gender_identity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(woma_su_t12, woman_female_category__gender_identity_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(woma_su_t16, woman_female_category__gender_identity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the woman_female_category kernel. sex_biology_reading authors category membership via chromosomal sex and developmental biology, with a structurally different beneficiary/victim set (protects natal-sex-conditioned space/sport access; burdens transgender individuals seeking recognition). hybrid_contextual_reading authors a context-dependent compromise (biology for medical/sports/safety, identity for social/legal recognition), with a narrower victim set on both sides since it attempts to avoid the direct clash. All three share the kernel_id woman_female_category and are linked bidirectionally via affects_constraints; do not treat any one file's epsilon as representative of 'the' constraint — each is a distinct, ε-invariant reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
