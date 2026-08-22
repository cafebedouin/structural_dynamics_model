% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Manifesto Settlement as Coerced Practice Suspension (Exogenous Override Reading)
 *   domain: religious/political
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto as an act of
 *   institutional surrender to federal force, not prophetic revelation.
 *   Facing corporate dissolution, confiscation of temples and property, mass
 *   imprisonment of practitioners, disfranchisement, and statehood held
 *   hostage, the church's first presidency suspended new plural marriages
 *   while maintaining that the doctrine requiring them remained in force. The
 *   arrangement modeled here is that settlement as this reading assesses it:
 *   a federally compelled practice ban, administered day-to-day by a coerced
 *   hierarchy, with the costs landing on members who had staked exaltation on
 *   the suspended covenant and on families whose existing structure was the
 *   object of the campaign. Claim and metrics are authored independently: the
 *   claimed type is tangled_rope (genuine survival-coordination plus
 *   asymmetric extraction); the metrics describe heavily extractive,
 *   coercively enforced operation whose enforcement locus migrated from
 *   Washington to Salt Lake City over the interval. KEY AGENTS (by structural
 *   relationship): - federal_government: Primary beneficiary/agenda-setter
 *   (institutional/arbitrage) — compels and collects compliance -
 *   anti_polygamy_coalition: Secondary beneficiary (organized/mobile) —
 *   collects vindication - lds_first_presidency: Dual-positioned
 *   administrator (institutional/identity_locked) — administers the
 *   suspension while paying in property, liberty, and doctrinal integrity -
 *   rank_and_file_latter_day_saints: Primary target
 *   (organized/identity_locked) — bears suspended-covenant costs -
 *   plural_marriage_families: Most exposed target (powerless/trapped) —
 *   existing families are the penalized object - post_manifesto_holdouts:
 *   Excluded dissenters (moderate/constrained) — purged when continuation
 *   surfaced - lds_historians_analytical: Analytical observer — sees the full
 *   documentary structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.72).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.75).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Manifesto Settlement as Coerced Practice Suspension (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious/political").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '57765707-961e-487f-8eac-0458f68cbd14').
narrative_ontology:cs_kernel_codification('57765707-961e-487f-8eac-0458f68cbd14', fixed_text).
narrative_ontology:cs_authority_grounding('57765707-961e-487f-8eac-0458f68cbd14', lineage).
narrative_ontology:cs_interpretation_layer_present('57765707-961e-487f-8eac-0458f68cbd14').
narrative_ontology:cs_reading_relation('57765707-961e-487f-8eac-0458f68cbd14', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('57765707-961e-487f-8eac-0458f68cbd14', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('57765707-961e-487f-8eac-0458f68cbd14', foundational, manifesto_was_coerced_capitulation_not_revelation).
narrative_ontology:cs_axiom_status(manifesto_was_coerced_capitulation_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('57765707-961e-487f-8eac-0458f68cbd14', manifesto_was_coerced_capitulation_not_revelation, empirically_contingent).
narrative_ontology:cs_axiom('57765707-961e-487f-8eac-0458f68cbd14', foundational, celestial_marriage_principle_remains_binding).
narrative_ontology:cs_axiom_status(celestial_marriage_principle_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('57765707-961e-487f-8eac-0458f68cbd14', celestial_marriage_principle_remains_binding, theological).
narrative_ontology:cs_reference_frame('57765707-961e-487f-8eac-0458f68cbd14', unrevoked_celestial_marriage_doctrine).
narrative_ontology:cs_drift_state('57765707-961e-487f-8eac-0458f68cbd14', post_amnesty_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57765707-961e-487f-8eac-0458f68cbd14', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_coalition).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_holdouts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the federal courts criminalized plural marriage, dissolved the church's corporate charter, seized its houses of worship and temporal property, disfranchised polygamists and Utah women, and made Utah statehood conditional on cessation. Prosecutions, fines, and confiscations continued until the church's governing council directed members to submit to the law. Once submission came, active enforcement wound down, but the sovereign retained the capacity to resume it at will.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% The church's governing presidency issued the declaration suspending new plural marriages, instructed members to obey the law, and later disciplined apostles and members who continued the practice or taught it as living doctrine. Its leaders had been imprisoned or driven underground in the preceding decade, and the corporation stood to lose its temples and remaining property. Suspending the practice was the price of institutional survival; the presidency administered that suspension internally while maintaining that the underlying doctrine remained true. Abandoning its position would have meant dissolving the institution itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_first_presidency, payer).

% Ordinary members had built their lives around the teaching that plural marriage was an eternal covenant required for exaltation. The declaration told them the practice must stop while the doctrine stayed in force; they carried the resulting tension between what they had been promised and what they were now required to do. Ward and stake structures kept them bound to the community; leaving meant losing family, standing, and the salvation framework itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, rank_and_file_latter_day_saints, payer,
    organized, biographical, identity_locked, regional).

% Families already formed under the practice faced prosecution, disfranchisement, and impoverishment; husbands served prison sentences or fled to Mexico and Canada, leaving households to subsist alone. New plural marriages were forbidden going forward. Their family arrangements were the precise object of the legal campaign, so no relocation or reclassification removed the jeopardy — only dissolving the families or permanent exile did.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_families, payer,
    powerless, biographical, trapped, regional).

% National reform organizations, Protestant churches, and both major parties had campaigned for a generation to extirpate plural marriage. The capitulation vindicated their campaign, confirmed their reading of the Constitution, and delivered the political capital of having forced a stubborn minority religion to yield. They dispersed afterward, their objective achieved.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_coalition, beneficiary,
    organized, biographical, mobile, national).

% Some apostles and members continued to authorize or enter plural marriages after the declaration, chiefly in Mexico and Canada, and continued to teach the principle as in force. When the continuation surfaced during the Senate investigation of Reed Smoot, the governing councils removed the noncompliant apostles from their quorum and excommunicated the most persistent practitioners. Their testimony that the practice had continued was unwelcome in official accounts; they were pushed out of the conversation and then out of the community.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_holdouts, excluded,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_holdouts, payer).

% Later scholars working from diaries, court records, sealing registers, and mission archives reconstruct the sequence of prosecutions, negotiations, and post-declaration marriages. Their accounts sit outside the institutions involved and can compare the official narrative against the documentary record.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_historians_analytical, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the jurisdictional collision between federal criminal law and LDS covenantal practice: establishes one national marriage standard, halts a prosecution-and-confiscation spiral, and preserves the church corporation by aligning practice with statute.
% TRANSFER_FUNCTION: Moves practice rights and institutional autonomy from the church to federal authority; moves compliance and legitimacy costs (suspended covenants, broken family-formation expectations, discipline of dissenters) onto members and plural families; moves political vindication to the anti-polygamy coalition and electoral normalization along Utah's path to statehood.
% ABSENT_VOICES: Plural wives and husbands — the people whose marriages were the object of the entire campaign — had no seat in the negotiation between federal authorities and the first presidency; noncompliant apostles who knew post-declaration marriages continued were removed from councils before their testimony could shape the official record during the Smoot investigation.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, either federal prosecution resumes or plural practice resumes; Utah's statehood terms, the church's corporate form, and the boundary between federal law and religious practice all depend on it. Today the deeper dependency is doctrinal: generations have been raised inside the monogamous settlement, so removal would force wholesale rearrangement or schism.
% FOUNDING_PROBLEM: Survive an escalating federal campaign — the Edmunds and Edmunds-Tucker Acts, prosecution of practitioners, confiscation of corporate property and temples, disfranchisement, and statehood conditioned on cessation — that would destroy the institution unless plural marriage practice ceased.
% FOUNDING_PROBLEM_CORROBORATION: Federal sources outside the church attest both the emergency and its end: the Edmunds-Tucker confiscation provisions, the Supreme Court rulings sustaining them, and President Harrison's 1893 amnesty proclamation declaring the practice substantially discontinued. Documentary historians working outside all benefiting parties (court records, mission archives, private diaries) corroborate that the coercive emergency ended while the suspension persisted and hardened into internal orthodoxy. No source outside the arrangement's own administrators attests that the founding emergency remained live after statehood.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.72 with the referent fixed to the standing suspension arrangement as this reading assesses it: practice rights transferred to the sovereign, covenantal promises suspended without repudiation, legitimacy costs carried by members. Suppression 0.75 is authored as a raw structural property (unscaled by power or scope; only extractiveness is scaled downstream, by directionality and scope). The suppression_requirement series is deliberately non-monotonic: federal enforcement relaxes after statehood, then the Second Manifesto re-ratchets enforcement above the relaxed baseline as the church adopts the suppressive function itself — enforcement-capacity change is the dynamic this story traces. Theater_ratio rises from 0.22 to 0.58 as official activity shifts from operating the settlement to maintaining the revelation-framing against the documentary record (denials during the Smoot hearings, post-purge narrative consolidation). Accessibility_collapse 0.65: alternatives collapsed into orthodoxy — exit means schism. Resistance 0.45: real and sustained (Mexican and Canadian colonies, noncompliant apostles, later fundamentalist schism) but never able to reopen the question. All three tracked series share one six-point grid. The coercion grid records the interval's central dynamic — a gradient flip: structural-level suppression collapses (0.90 to 0.45, retaining only latent sovereign capacity) while organizational-level suppression rises (0.70 to 0.78); the scalar suppression summarizes the arrangement's enforcement core across the whole interval, so it sits above the endpoint level-average. Coalition note: members were organized through ward and stake structures, but identity-lock kept that organizing capacity from converting into resistance against the settlement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural inputs. The federal seat experiences the settlement as successful law enforcement and collects compliance (near-beneficiary directionality); the anti-polygamy coalition collects vindication and disperses. The hierarchy seat is dual-positioned: it administers the suspension (agenda-setting pull toward the beneficiary end) while paying in imprisoned leaders, seized property, and doctrinal integrity (payer pull toward the target end); the derivation lands it mid-range, which is why no directionality override is used — an override keyed to the institutional power atom would also strike the federal seat. Member and plural-family seats sit near the full-target end, with trapped and identity_locked exits pushing them further: plural families cannot exit without dissolving the very families the arrangement penalizes; members' exit is identity loss. Identity-lock mechanisms differ by seat: for the hierarchy it is institutional identity (the presidency has become its custodial function); for members it is ideological-relational fusion (exaltation promised through the suspended covenant). If the identity frame broke — if members widely recognized the settlement as pure duress — internal enforcement would fail and the arrangement would migrate toward inertial maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: federal_government (collects cessation, uniform marriage law, territorial control) and anti_polygamy_coalition (collects vindication) derive low d. Victim declarations: rank_and_file_latter_day_saints, plural_marriage_families, and post_manifesto_holdouts derive high d, amplified by exit profiles (trapped, identity_locked, constrained). The hierarchy's dual role (agenda_setter/payer) places it between seats; its identity_locked exit keeps it away from the beneficiary end despite administering the arrangement. Scope assignments follow where the arrangement actually binds: member seats regional, the federal seat national, the hierarchy continental (missions abroad), the observer global.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an existential federal-church collision threatening corporate dissolution — died with the amnesty proclamation (1893) and statehood (1896), yet the suspension persisted and hardened into internal orthodoxy. Authoring founding_problem_status=dead alongside disappearance_verdict=world_rearranges surfaces exactly the mismatch the R5 consumer flags: an arrangement persisting past its mandate, maintained by the administrator of its own enforcement. The tangled_rope claim prevents two mislabels: a rope reading would erase the identifiable victims (plural families, disciplined members); a snare reading would erase the genuine coordination function (the settlement really did solve the church's survival problem and settle state-church jurisdiction). The measurement series dates the drift: external suppression decays while theater rises — by interval end the arrangement trends toward inertial, theatrically maintained settlement, hence mandatrophy_resolved is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the marriage_commitment_legitimacy kernel. What would the sibling readings change structurally if instantiated?',
    'Generate the endogenous and hybrid readings as separate stories and compare epsilon, beneficiary/victim sets, and computed types; the endogenous reading relocates authorship to divine command (no federal beneficiary, different victim set); the hybrid splits agency between exogenous crisis and managerial response.',
    'Classification, gain_flow, and fixing_cost are indexical to the reading; cross-reading comparison is the corpus-level measurement, not a defect to reconcile within this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexed classification of a three-reading kernel.').

omega_variable(
    duress_vs_revelation_documentary_record,
    'Does the documentary record (Woodruff''s private papers, contemporaneous correspondence, sequencing against court decisions and confiscation deadlines) establish duress rather than revelation as the operative cause?',
    'Archival publication and scholarly consilience on the private record versus the public framing.',
    'Strengthens or weakens this reading''s foundational empirical axiom; a revelation-favoring record would push the story toward the endogenous sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_vs_revelation_documentary_record, empirical, 'Evidentiary status of the coercion claim.').

omega_variable(
    post_declaration_marriage_extent,
    'How many plural marriages were performed with ecclesiastical authorization after the declaration and before the second manifesto?',
    'Sealing-register reconstruction and demographic analysis of Utah, Mexican, and Canadian colony records.',
    'Calibrates theater_ratio (official denial versus continued practice) and the suppression series'' ratchet at the second manifesto.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_declaration_marriage_extent, empirical, 'Extent of post-declaration practice bearing on theater and enforcement timing.').

omega_variable(
    persistence_after_duress_mechanism,
    'Why does the suspension persist after the duress ends — internalized orthodoxy, institutional identity, latent federal capacity, or leader preference?',
    'Compare enforcement behavior across periods of varying latent federal attention; analyze leader discourse after amnesty.',
    'Determines whether the late-interval arrangement trends toward inertial maintenance (piton-ward) or retains active stakes (tangled_rope steady-state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_after_duress_mechanism, conceptual, 'Mechanism of post-duress persistence.').

omega_variable(
    legitimacy_cost_distribution,
    'Who actually bore the gap-recognition costs — broad rank-and-file crisis, concentrated plural-family harm, or elite-level awareness only?',
    'Diaries, sermons, and disciplinary records across wards and stakes; demographic stress markers in plural-family households.',
    'Redistributes effective extraction across member seats; a concentrated-victims finding raises effective extraction for plural families relative to the member body.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_cost_distribution, empirical, 'Distribution of legitimacy-crisis costs across member strata.').

omega_variable(
    coercion_grid_level_judgment_uncertainty,
    'The coercion grid''s class and individual cells rest on thinner documentation than the structural and organizational cells; how much confidence do the level-resolved values deserve?',
    'Ward-level disciplinary records and household-level legal-case sampling to firm up class and individual intensities.',
    'If class-level suppression at interval end is lower than authored, the gradient-flip finding strengthens; if higher, the organizational takeover was less complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_grid_level_judgment_uncertainty, conceptual, 'Authoring uncertainty in level-resolved grid cells.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_exog_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(manifesto_exog_tr_t7, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(manifesto_exog_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.46).
narrative_ontology:measurement(manifesto_exog_tr_t21, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 21, 0.52).
narrative_ontology:measurement(manifesto_exog_tr_t28, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 28, 0.56).
narrative_ontology:measurement(manifesto_exog_tr_t34, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 34, 0.58).

% Extraction over time
narrative_ontology:measurement(manifesto_exog_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(manifesto_exog_be_t7, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 7, 0.77).
narrative_ontology:measurement(manifesto_exog_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.75).
narrative_ontology:measurement(manifesto_exog_be_t21, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 21, 0.73).
narrative_ontology:measurement(manifesto_exog_be_t28, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 28, 0.72).
narrative_ontology:measurement(manifesto_exog_be_t34, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 34, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_exog_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(manifesto_exog_su_t7, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 7, 0.78).
narrative_ontology:measurement(manifesto_exog_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.85).
narrative_ontology:measurement(manifesto_exog_su_t21, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 21, 0.82).
narrative_ontology:measurement(manifesto_exog_su_t28, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 28, 0.78).
narrative_ontology:measurement(manifesto_exog_su_t34, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 34, 0.75).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=34
narrative_ontology:measurement(manifesto_exog_grid_01, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(manifesto_exog_grid_02, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 34, 0.7).
narrative_ontology:measurement(manifesto_exog_grid_03, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 0, 0.6).
narrative_ontology:measurement(manifesto_exog_grid_04, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 34, 0.65).
narrative_ontology:measurement(manifesto_exog_grid_05, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.8).
narrative_ontology:measurement(manifesto_exog_grid_06, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 34, 0.65).
narrative_ontology:measurement(manifesto_exog_grid_07, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(manifesto_exog_grid_08, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 34, 0.6).
narrative_ontology:measurement(manifesto_exog_grid_09, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 0, 0.6).
narrative_ontology:measurement(manifesto_exog_grid_10, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 34, 0.5).
narrative_ontology:measurement(manifesto_exog_grid_11, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(manifesto_exog_grid_12, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 34, 0.45).
narrative_ontology:measurement(manifesto_exog_grid_13, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 0, 0.5).
narrative_ontology:measurement(manifesto_exog_grid_14, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 34, 0.45).
narrative_ontology:measurement(manifesto_exog_grid_15, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 0, 0.4).
narrative_ontology:measurement(manifesto_exog_grid_16, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 34, 0.3).
narrative_ontology:measurement(manifesto_exog_grid_17, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 0, 0.8).
narrative_ontology:measurement(manifesto_exog_grid_18, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 34, 0.45).
narrative_ontology:measurement(manifesto_exog_grid_19, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 0, 0.75).
narrative_ontology:measurement(manifesto_exog_grid_20, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 34, 0.3).
narrative_ontology:measurement(manifesto_exog_grid_21, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 0, 0.85).
narrative_ontology:measurement(manifesto_exog_grid_22, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 34, 0.4).
narrative_ontology:measurement(manifesto_exog_grid_23, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 0, 0.9).
narrative_ontology:measurement(manifesto_exog_grid_24, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 34, 0.4).
narrative_ontology:measurement(manifesto_exog_grid_25, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 0, 0.75).
narrative_ontology:measurement(manifesto_exog_grid_26, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 34, 0.65).
narrative_ontology:measurement(manifesto_exog_grid_27, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 0, 0.7).
narrative_ontology:measurement(manifesto_exog_grid_28, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 34, 0.6).
narrative_ontology:measurement(manifesto_exog_grid_29, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 0, 0.7).
narrative_ontology:measurement(manifesto_exog_grid_30, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 34, 0.78).
narrative_ontology:measurement(manifesto_exog_grid_31, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 0, 0.9).
narrative_ontology:measurement(manifesto_exog_grid_32, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 34, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto' decomposes into three structurally distinct constraints — one per reading of the marriage_commitment_legitimacy kernel. This file carries the exogenous_override_reading (high epsilon: federal beneficiary, member victims). The endogenous_reinterpretation_reading carries a different epsilon and beneficiary set (divine command; no federal rent-collector), and the hybrid_pragmatic_reading splits agency between exogenous crisis and managerial response. Each story links the others via affects_constraints; epsilon differences across the family are the measurement, not noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
