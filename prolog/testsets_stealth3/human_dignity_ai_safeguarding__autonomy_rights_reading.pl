% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: human_dignity_ai_safeguarding__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Reading of Human Dignity in AI Safeguarding
 *   domain: theological ethics / technology governance / philosophical anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the human_dignity_ai_safeguarding
 *   kernel: the autonomy-rights reading, on which dignity is grounded in
 *   human autonomy, rationality, and enforceable rights rather than in a
 *   divine image or an open constitution of personhood. As realized in AI and
 *   data governance, the reading produces a regulatory arrangement that
 *   conditions market access on transparency, consent, labor and privacy
 *   protection, and recourse mechanisms, while permitting enhancement only
 *   within rights limits. The epsilon referent is the standing implemented
 *   arrangement — the actual consent, assessment, and enforcement machinery
 *   as it operates — assessed by this reading's own lights, never the
 *   idealized rights order the reading endorses. The claim (tangled_rope) and
 *   the metrics are independently authored facts: the claim states the
 *   structure I believe true (genuine coordination function plus asymmetric
 *   incidence plus active enforcement); the metrics state what I believe
 *   descriptively accurate of the arrangement's operation. Committer content
 *   — the kernel contest, sibling deltas, and where the disagreement is
 *   located — is routed to the omega variables, not averaged into this file.
 *
 * KEY AGENTS:
 *   - data_subjects and algorithmically_managed_workers: primary protected beneficiaries (powerless/trapped and organized/constrained) — receive disclosure, consent, labor-protection, and recourse provisions without running the machinery
 *   - compliance_services_industry: collecting beneficiary (powerful/mobile) — sells the assessments, certifications, and remediation the obligations require; grows with every new duty
 *   - incumbent_ai_platforms: dual-positioned beneficiary-payer (institutional/arbitrage) — pays large compliance sums yet converts fixed overhead into a moat that thins the competitive field
 *   - independent_ai_developers and open_source_ai_contributors: primary targets (moderate/constrained) — bear disproportionate fixed compliance burdens against small revenue bases
 *   - data_protection_regulators: agenda setter (institutional/constrained) — drafts, administers, and enforces the translation of dignity-as-rights into technical requirements
 *   - enhancement_advocates: excluded voice (moderate/identity_locked) — holds that individuals should author their own embodiment; sits outside the advisory and standard-setting bodies where the rules are written
 *   - civil_society_rights_watchdogs: analytical observer (organized/analytical) — audits deployed systems and supplies the evidence base on whether the rights machinery delivers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.52).
domain_priors:suppression_score(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.55).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__autonomy_rights_reading, "Autonomy-Rights Reading of Human Dignity in AI Safeguarding").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__autonomy_rights_reading, "theological ethics / technology governance / philosophical anthropology").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__autonomy_rights_reading, 'cc77adaa-623c-468b-a894-03740000ff4c').
narrative_ontology:cs_kernel_codification('cc77adaa-623c-468b-a894-03740000ff4c', fixed_text).
narrative_ontology:cs_authority_grounding('cc77adaa-623c-468b-a894-03740000ff4c', lineage).
narrative_ontology:cs_interpretation_layer_present('cc77adaa-623c-468b-a894-03740000ff4c').
narrative_ontology:cs_reading_relation('cc77adaa-623c-468b-a894-03740000ff4c', human_dignity_ai_safeguarding__imago_dei_reading, forecloses).
narrative_ontology:cs_reading_relation('cc77adaa-623c-468b-a894-03740000ff4c', human_dignity_ai_safeguarding__posthumanist_reading, coexists_with).
narrative_ontology:cs_axiom('cc77adaa-623c-468b-a894-03740000ff4c', foundational, dignity_grounded_in_rational_autonomy).
narrative_ontology:cs_axiom_status(dignity_grounded_in_rational_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('cc77adaa-623c-468b-a894-03740000ff4c', dignity_grounded_in_rational_autonomy, deontological).
narrative_ontology:cs_axiom('cc77adaa-623c-468b-a894-03740000ff4c', secondary, rights_enforcement_realizes_dignity).
narrative_ontology:cs_axiom_status(rights_enforcement_realizes_dignity, holdable).
narrative_ontology:cs_axiom_grounding('cc77adaa-623c-468b-a894-03740000ff4c', rights_enforcement_realizes_dignity, instrumental).
narrative_ontology:cs_reference_frame('cc77adaa-623c-468b-a894-03740000ff4c', postwar_universal_rights_settlement).
narrative_ontology:cs_drift_state('cc77adaa-623c-468b-a894-03740000ff4c', contemporary_datafication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc77adaa-623c-468b-a894-03740000ff4c', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, data_subjects).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, algorithmically_managed_workers).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_services_industry).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, independent_ai_developers).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, open_source_ai_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the rules that translate dignity-as-rights into technical requirements: consent standards, disclosure duties, risk-tiered obligations, audit powers, and fines. Their budgets and staffing grow with the scope of the regime they administer. They answer to legislatures and courts, and their enforcement priorities determine which obligations bind in practice and which remain letterhead.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, data_protection_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Persons whose data feeds AI systems and whose lives are touched by automated decisions. The regime grants them disclosure, consent levers, objection rights, and complaint channels they did not previously hold. Individually they have little bargaining power and rarely exercise the rights the paperwork provides; opting out of datafied services altogether is not a realistic way to live.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, data_subjects, beneficiary,
    powerless, biographical, trapped, global).

% Workers scheduled, rated, priced, and dispatched by algorithmic systems. The labor-protection provisions give them advance notice, explanations, and human-review hooks for consequential decisions. Collective organization through unions gives them a voice individual complaints lack; leaving platform work means losing income in the meantime.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, algorithmically_managed_workers, beneficiary,
    organized, biographical, constrained, global).

% Consultancies, audit firms, and notified bodies selling gap analyses, impact assessments, conformity certification, and remediation advice. Every new obligation enlarges their addressable market, and recurring assessment cycles convert one-off duties into annuity revenue. Their commercial interest favors maximal procedural complexity, and the fee stream from conformity work concentrates here.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_services_industry, beneficiary,
    powerful, biographical, mobile, global).

% Large operators with in-house legal and engineering teams that absorb fixed compliance costs far more cheaply than rivals, hold seats on the standards bodies that draft the rules, and gain relative advantage as overhead thins the field of smaller competitors. They also write large checks: penalties, assessments, and re-engineering for compliance. Their multi-jurisdiction footprint lets them shift activity toward favorable regimes when rules tighten.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, payer).

% Small studios and startups shipping AI products without dedicated compliance staff. The fixed costs of impact assessments, technical documentation, and certification weigh heavily against small revenue bases. Some delay launches in regulated markets, some narrow their products to dodge regulated categories, and some never enter at all.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, independent_ai_developers, payer,
    moderate, biographical, constrained, global).

% Distributed communities releasing models and tooling without charge. Liability and documentation duties attach downstream, but upstream contributors face mounting pressure to embed provenance records, usage restrictions, and evaluation reports in releases, and several jurisdictions debate extending publisher duties to model releasers. Their informal, volunteer coordination sits awkwardly inside a regime built for corporate accountability chains.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, open_source_ai_contributors, payer,
    moderate, biographical, constrained, global).

% Transhumanist bioethicists, self-experimenters, and cognition- and longevity-enhancement ventures who hold that individuals should author the shape of their own bodies and minds. The regime's caution toward enhancement — permitted only within rights limits — treats their project as a risk category rather than a constituency. They sit outside the advisory boards and standard-setting processes where the lines are drawn, and their position is constitutive of their worldview rather than a negotiable preference.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, enhancement_advocates, excluded,
    moderate, generational, identity_locked, global).

% NGOs and academic labs that audit deployed systems, stress-test consent interfaces, document discriminatory outcomes, and bring test litigation. They assess whether the rights machinery delivers what it promises and supply much of the evidence base regulators and journalists cite.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, civil_society_rights_watchdogs, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_services_industry).
narrative_ontology:fixing_cost_class(human_dignity_ai_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust problem of putting AI systems into social life at scale: standardized disclosure, consent, risk assessment, and recourse let persons interact with automated systems without individually verifying each system's behavior, and common rights standards let data and services cross borders on predictable terms.
% TRANSFER_FUNCTION: Moves compliance expenditure and data-control concessions from AI developers and operators toward protected persons (as realized rights) and toward the assessment and enforcement apparatus (certification fees, consultancy revenue, agency budgets); moves market access from non-compliant to compliant actors.
% ABSENT_VOICES: Enhancement advocates and posthumanist-aligned researchers would object that the frame's cautious enhancement boundary forecloses self-authorship of body and mind, yet they hold no seat in the advisory or standard-setting bodies. Populations outside covered jurisdictions are also absent: their data flows into covered markets without equivalent protections, and their consent is mediated by contracts they had no hand in shaping.
% DISAPPEARANCE_RATIONALE: If the rights-based safeguarding regime vanished overnight, data markets would reorganize around collection-by-default, algorithmic management would shed its review hooks and notice duties, the assessment and certification industry would lose its revenue base, and the trust infrastructure that lets strangers accept automated decisions would have to be rebuilt from scratch — deployments, pricing, and labor arrangements would all shift within quarters.
% FOUNDING_PROBLEM: Mid-twentieth-century atrocities drove the postwar settlement that grounded human dignity in universal, equal, rights-bearing personhood rather than in divine image or rank. The datafication and AI waves then created a new capability — automated decision-making over persons at scale — that required translating that rights-grounded dignity into technical governance: consent, disclosure, labor protection, and recourse against systems that decide.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: investigative reporting on algorithmic management harms in delivery, warehouse, and gig work; worker testimony gathered by unions and labor inspectorates; peer-reviewed audits documenting discriminatory outcomes in deployed systems; and parliamentary inquiry records. None of these seats sits inside the compliance industry or the enforcement budget the arrangement funds, and their continued documentation of live harm attests that the founding problem persists in new technical form.
narrative_ontology:disappearance_verdict(human_dignity_ai_safeguarding__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_dignity_ai_safeguarding__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_dignity_ai_safeguarding__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 because the standing arrangement genuinely transfers resources — compliance expenditure, assessment fees, enforcement budgets, market access — and part of that transfer tracks protection while part tracks procedure. Suppression is 0.55, matching the reading's designed moderation: enforcement gates deployment paths and market access rather than eliminating alternatives outright; offshore routing, open-weight releases, and non-covered jurisdictions persist, hence accessibility_collapse at 0.48. Theater_ratio at 0.42 reflects the documented growth of formalistic consent interfaces and boilerplate impact assessments alongside still-functioning complaint, audit, and fining channels. Resistance at 0.58 reflects sustained industry lobbying, jurisdictional arbitrage, and open-community friction against extending duties to model publishers. The temporal series run on one shared seven-point grid (1948–2026) so every tracked metric is authored at every examined time point; all three series rise monotonically — extraction accumulates as compliance machinery layers onto the original declaratory settlement, theater climbs as interaction scales past any deliberative capacity, and suppression_requirement climbs as the enforcement apparatus (authorities with fining powers, conformity-assessment regimes, audit duties) is deliberately built up. There is no oscillation to model: the drift is ratchet-shaped, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the data-subject seat the arrangement is protection: disclosure and recourse that did not exist before. From the independent-developer seat the same structure is a fixed tax that decides market entry. From the incumbent seat it is a moat purchased at a price they can afford and rivals cannot. From the regulator seat it is a mandate whose budget grows with its scope. From the compliance-industry seat it is an addressable market that expands with every amendment. From the excluded enhancement advocate it is a closed door. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects and algorithmically managed workers are declared beneficiaries with trapped or constrained exit — the arrangement subsidizes them, placing their derived directionality near the beneficiary end. The compliance services industry is a pure collector with mobile exit — nearest the beneficiary end. Incumbent platforms carry a dual declaration (beneficiary with payer secondary role) and arbitrage-grade exit: they pay, but the net structural flow runs toward them, so their derived d stays low despite the checks they write. Independent developers and open-source contributors are declared victims with constrained exit — near the target end, amplified by the regime's continental-to-global scope making verification and proportionate relief harder. Regulators administer the arrangement and collect budgets through it; their derived d sits below symmetric. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the override mechanism keys on power atoms that would conflate the distinct institutional seats (regulator vs. incumbent) rather than separate them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — automated decision-making over persons continues to expand — so no mandatrophy is declared. The tangled_rope classification earns its keep by blocking two symmetrical misreadings. Reading the whole apparatus as pure extraction erases the real coordination function: standardized trust infrastructure, cross-border data-flow predictability, and recourse channels that no participant could provision alone. Reading it as pure coordination erases the documented asymmetry: fixed compliance costs that regress against firm size, an assessment industry whose revenue scales with procedural complexity, and consent interfaces that satisfy procedure more often than they transfer control. The rising theater_ratio series is the early-warning instrument here: if consent validity (see omega) resolves as largely fictional while the fee streams persist, the arrangement is drifting from tangled rope toward snare with the coordination story as cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the human_dignity_ai_safeguarding kernel. How would the sibling readings — imago_dei_reading (dignity as divine image, equal in all persons prior to any capability) and posthumanist_reading (dignity attaching to persons however constituted, including enhanced or synthetic) — change the constraint''s structure?',
    'Adjudication in constitutional, legislative, and international fora over which dignity ground governs AI safeguarding; observe which reading''s provisions are adopted in binding instruments and which readings retain live institutional sponsorship.',
    'Under the imago-dei reading, capability-conditioned eligibility tests (consent capacity, rationality thresholds) lose legitimacy and the protected set expands to all persons regardless of capability. Under the posthumanist reading, standing extends to enhanced and synthetic persons and the enhancement-permission boundary widens. Either switch alters the beneficiary and victim sets, the epsilon referent''s content, and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: which dignity ground governs AI safeguarding, and what each sibling reading would structurally change.').

omega_variable(
    consent_mechanism_validity,
    'Does the consent machinery produce meaningful autonomous authorization, or formalistic click-through that satisfies procedure without transferring control to the person consenting?',
    'Behavioral studies measuring comprehension and deliberation in consent flows; dark-pattern audits of consent interfaces; comparison of exercised opt-out rates against expressed preferences.',
    'If consent is largely formalistic, a large share of the measured protection is performative and the arrangement''s cost side (compliance spend without corresponding control transfer) dominates — pushing the computed classification toward the snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_validity, empirical, 'Whether consent interfaces deliver real autonomy or procedural fiction.').

omega_variable(
    compliance_cost_incidence,
    'Does the fixed-cost compliance burden fall disproportionately on small and independent developers relative to the protective benefit the regime delivers to them and to data subjects?',
    'Cost-of-compliance surveys stratified by firm size; market-entry and concentration analysis before and after major regulatory milestones.',
    'Confirmed regressive incidence strengthens the asymmetric-incidence half of the tangled-rope structure and identifies the incumbent moat as a capture channel; flat incidence would support a purer coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Regressivity of compliance burden across developer scale.').

omega_variable(
    enhancement_permission_boundary,
    'Where should the rights frame draw the line between protected bodily and mental integrity and permitted self-modification — and is the current cautious-permission boundary a considered rights judgment or an unexamined status-quo preference?',
    'Deliberative and legislative processes that include enhancement constituencies rather than only risk-framed expert panels; track boundary movement as enhancement technologies mature and as excluded advocates gain standing.',
    'A wider permission boundary converts currently suppressed activities into coordinated ones and moves the excluded constituency into the beneficiary set; a narrower boundary raises suppression and deepens the exclusion documented in absent_voices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_permission_boundary, preference, 'Contested placement of the enhancement-permission line within the rights frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__autonomy_rights_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1948, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 1948, 0.06).
narrative_ontology:measurement(huma_tr_t1970, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(huma_tr_t1990, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(huma_tr_t2005, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(huma_tr_t2016, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement(huma_tr_t2021, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(huma_tr_t2026, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t1948, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(huma_be_t1970, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 1970, 0.26).
narrative_ontology:measurement(huma_be_t1990, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(huma_be_t2005, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(huma_be_t2016, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement(huma_be_t2021, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 2021, 0.49).
narrative_ontology:measurement(huma_be_t2026, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1948, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 1948, 0.08).
narrative_ontology:measurement(huma_su_t1970, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 1970, 0.14).
narrative_ontology:measurement(huma_su_t1990, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(huma_su_t2005, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement(huma_su_t2016, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 2016, 0.46).
narrative_ontology:measurement(huma_su_t2021, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement(huma_su_t2026, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__imago_dei_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__posthumanist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'human dignity in AI ethics' decomposes into three structurally distinct constraints sharing one kernel: this autonomy-rights reading, the imago-dei reading, and the posthumanist reading. They differ in epsilon because they differ in who counts as a rights-holder and what arrangements count as violations. The imago-dei reading renders capability comparisons irrelevant to standing, dissolving the consent-capacity and rationality-threshold machinery this reading depends on. The posthumanist reading extends standing to enhanced and synthetic persons, widening the protected class and loosening the enhancement boundary this reading keeps cautious. This reading conditions standing on autonomy and rationality and therefore generates the consent, privacy, and labor apparatus authored here. Upstream/downstream structure: the postwar rights codification (this reading's lineage) supplies the textual and institutional substrate the other two readings argue against, so this file links both siblings via affects_constraints; each sibling file reciprocates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
