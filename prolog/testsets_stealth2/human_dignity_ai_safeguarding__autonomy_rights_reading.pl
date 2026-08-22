% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   human_dignity_ai_safeguarding: the autonomy-rights reading, under which
 *   human dignity in AI governance is secured through rights-based
 *   instruments — consent, transparency, labor and privacy protection —
 *   grounded in human autonomy and rationality rather than theological
 *   status. The standing arrangement under contest is the existing
 *   rights-based safeguarding regime as it operates: mandatory impact
 *   assessments, consent infrastructures, algorithmic-management labor
 *   provisions, neural-data protections, and the enforcement apparatus
 *   administering them. Per the epsilon-referent rule, extractiveness is
 *   authored for THAT arrangement as this reading assesses it, never for the
 *   arrangements sibling readings would install. The sibling readings
 *   (imago_dei_reading, posthumanist_reading) are separate constraint files
 *   linked through network.affects_constraints; their content is deliberately
 *   not described here. Claim/metric independence is preserved: the claimed
 *   type states what this reading believes is structurally true — a genuine
 *   coordination core wrapped around real, asymmetrically distributed
 *   compliance extraction — while the metrics state what is descriptively
 *   true of the regime's operation. Where the engine computes per-seat types
 *   diverging from the claim, that divergence is the datum the corpus exists
 *   to take.
 *
 * KEY AGENTS:
 *   - - data_subjects_and_users: Protected principal (moderate/constrained) — receives consent and recourse rights, bears indirect costs
 *   - - algorithmically_managed_workers: Primary intended beneficiary (powerless/constrained) — labor provisions target their situation
 *   - - privacy_rights_advocacy_institutions: Organized beneficiary (organized/mobile) — standing and agenda expand with the framework
 *   - - compliance_audit_industry: Fee-collecting beneficiary (organized/mobile) — revenue scales with obligation volume
 *   - - incumbent_ai_platforms: Dual-positioned payer-beneficiary (institutional/arbitrage) — pays the largest absolute bills, captures the moat
 *   - - early_stage_ai_startups: Disproportionate payer (moderate/constrained) — fixed overhead before revenue
 *   - - neurotech_enhancement_researchers: Constrained payer (moderate/constrained) — permitted-but-costly enhancement path
 *   - - data_protection_authorities: Agenda setter (institutional/identity_locked) — administers the enforcement pipeline
 *   - - open_source_ai_contributors: Excluded voice (moderate/mobile) — outside consultation tables
 *   - - informal_sector_algorithmic_workers: Excluded voice (powerless/trapped) — outside the regime's reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42).
domain_priors:suppression_score(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.45).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__autonomy_rights_reading, "Autonomy-Rights Reading of Human Dignity in AI Safeguarding").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__autonomy_rights_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__autonomy_rights_reading, '9f744032-82a9-4bb9-8888-3de98c2a1dc1').
narrative_ontology:cs_kernel_codification('9f744032-82a9-4bb9-8888-3de98c2a1dc1', fixed_text).
narrative_ontology:cs_authority_grounding('9f744032-82a9-4bb9-8888-3de98c2a1dc1', lineage).
narrative_ontology:cs_interpretation_layer_present('9f744032-82a9-4bb9-8888-3de98c2a1dc1').
narrative_ontology:cs_reading_relation('9f744032-82a9-4bb9-8888-3de98c2a1dc1', human_dignity_ai_safeguarding__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f744032-82a9-4bb9-8888-3de98c2a1dc1', human_dignity_ai_safeguarding__posthumanist_reading, influences).
narrative_ontology:cs_axiom('9f744032-82a9-4bb9-8888-3de98c2a1dc1', foundational, dignity_ground_is_autonomy_and_rationality).
narrative_ontology:cs_axiom_status(dignity_ground_is_autonomy_and_rationality, holdable).
narrative_ontology:cs_axiom_grounding('9f744032-82a9-4bb9-8888-3de98c2a1dc1', dignity_ground_is_autonomy_and_rationality, deontological).
narrative_ontology:cs_axiom('9f744032-82a9-4bb9-8888-3de98c2a1dc1', foundational, rights_instruments_suffice_to_protect_dignity).
narrative_ontology:cs_axiom_status(rights_instruments_suffice_to_protect_dignity, holdable).
narrative_ontology:cs_axiom_grounding('9f744032-82a9-4bb9-8888-3de98c2a1dc1', rights_instruments_suffice_to_protect_dignity, instrumental).
narrative_ontology:cs_reference_frame('9f744032-82a9-4bb9-8888-3de98c2a1dc1', autonomy_centered_rights_order).
narrative_ontology:cs_drift_state('9f744032-82a9-4bb9-8888-3de98c2a1dc1', contemporary_ai_capability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f744032-82a9-4bb9-8888-3de98c2a1dc1', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, data_subjects_and_users).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, algorithmically_managed_workers).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, privacy_rights_advocacy_institutions).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_audit_industry).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, early_stage_ai_startups).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, neurotech_enhancement_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, autonomy_as_dignity_ground).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, procedural_rights_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use AI-mediated services for credit, hiring, healthcare triage, and content. The regime gives them consent prompts, explanation rights, and complaint channels; exercising those channels takes time and rarely changes an individual outcome, and opting out of AI-mediated services altogether usually means exiting essential services.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, data_subjects_and_users, beneficiary,
    moderate, biographical, constrained, global).

% Work under scheduling, pricing, and evaluation algorithms on delivery, ride-hail, and warehouse platforms. Labor-protection provisions give them transparency about automated decisions and avenues to contest deactivations. Individual leverage is thin; income depends on continued platform access, so leaving a platform means lost earnings while they search for alternative work.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, algorithmically_managed_workers, beneficiary,
    powerless, immediate, constrained, global).

% Litigate, lobby, and publish around data protection and algorithmic fairness. Each expansion of the rights framework enlarges their standing, funding base, and agenda. They operate across jurisdictions and can shift attention to whichever forum is most receptive.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, privacy_rights_advocacy_institutions, beneficiary,
    organized, generational, mobile, continental).

% Sell documentation, impact-assessment, auditing, and certification services that deployers are required to obtain. Revenue scales with the volume and complexity of obligations; the industry grows as new duties are added and has no equivalent revenue stream if obligations shrink.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Operate large consumer AI products and carry the largest absolute compliance bills: legal teams, audit contracts, explainability engineering, consent infrastructure. They also shape rulemaking through consultations and standards bodies, and their scale lets them absorb fixed compliance costs that raise rivals' relative costs. Compute and corporate structures can be shifted across jurisdictions when rules bind tightly in one.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, beneficiary).

% Build new models and applications with small teams and runway measured in months. Every obligation adds fixed legal and process overhead before revenue arrives; investors steer portfolios toward compliant markets, and selling into regulated customer segments requires certifications the startup must fund upfront. Relocating to lighter jurisdictions cuts them off from customers and capital.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, early_stage_ai_startups, payer,
    moderate, immediate, constrained, national).

% Develop cognitive-enhancement devices and interventions under a permission structure that allows cautious enhancement inside consent, safety, and privacy constraints. Trial approvals, neural-data protections, and review boards add years and cost; moving to permissive jurisdictions forfeits publication venues, partners, and reimbursement pathways anchored in regulated markets.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, neurotech_enhancement_researchers, payer,
    moderate, generational, constrained, continental).

% Write guidance, issue fines, certify auditors, and run the enforcement pipeline. Their mandate, staffing, and public standing exist because the regime exists; dismantling it would dissolve the institution's reason to operate. Budgets grow with enforcement scope, and leadership careers are made inside the apparatus.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, data_protection_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Distributed volunteers and small labs release model weights and tooling outside corporate compliance departments. Consultation processes are built around incorporated deployers with legal representatives, so their objections about documentation burdens and license incompatibilities rarely enter the rooms where obligations are drafted.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, open_source_ai_contributors, excluded,
    moderate, biographical, mobile, global).

% Piecework and informal logistics workers managed through messaging groups, spreadsheet rosters, and off-the-shelf software in economies the formal regime does not reach. They experience the same automated scheduling and arbitrary deactivation the regime was built to address, but no obligation binds their employers and no channel represents them in rulemaking.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, informal_sector_algorithmic_workers, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_dignity_ai_safeguarding__autonomy_rights_reading, compliance_audit_industry).
narrative_ontology:fixing_cost_class(human_dignity_ai_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust and accountability problem of deploying AI into high-stakes domains: standardized consent, transparency, and audit infrastructure gives credit, hiring, medical, and workplace systems legible recourse channels, letting deployers, users, and regulators coordinate expectations without each dispute being renegotiated from scratch.
% TRANSFER_FUNCTION: Moves compliance expenditure — legal, audit, documentation, and engineering-for-explainability effort — from AI developers and deployers toward compliance service providers and the regulatory apparatus; moves enforceable bargaining power toward data subjects and algorithmically managed workers.
% ABSENT_VOICES: Informal-sector workers under unregulated algorithmic management sit outside the regime's reach entirely and are absent from rulemaking; open-source contributors are absent from consultation tables structured around incorporated deployers. Both would object that obligations are drafted for the formal economy's largest actors while the harms the regime targets continue unchecked elsewhere.
% DISAPPEARANCE_RATIONALE: Consent infrastructures, audit and certification markets, worker recourse channels, cross-border data-transfer mechanisms, and neurotech trial pipelines are all organized around the regime; overnight removal would strand certified processes, void transfer agreements, and force every deployer-user relationship back onto ad hoc negotiation.
% FOUNDING_PROBLEM: Mid-2010s onward: opaque algorithmic decision-making at scale — discriminatory scoring, exploitative gig management, mass behavioral profiling — outran existing legal categories, and pluralistic secular polities needed a dignity-protection instrument that did not depend on theological premises to command broad assent.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: court rulings finding automated-decision violations, independent academic audits of deployed commercial models documenting bias and opacity, and investigative reporting on algorithmic workplace harm. No outside source attests the founding problem is solved; the regime's own communications claiming maturity are the only sources doing so.
narrative_ontology:disappearance_verdict(human_dignity_ai_safeguarding__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_dignity_ai_safeguarding__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).
:- end_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42: by this reading's own lights most compliance cost is the legitimate price of rights-respecting deployment, but a growing share is rent — audit fees scaled to obligation complexity, fixed costs that favor incumbents, and bureaucratic self-expansion. Suppression 0.45 is moderate: binding mandates constrain design space and enhancement pace, but jurisdictional and architectural exits remain open, matching the expected structural delta. Theater 0.31 and rising: consent banners few read, boilerplate assessments, checkbox certification coexist with genuinely enforced fines and takedowns. Accessibility collapse 0.45: within a jurisdiction the compliance path is mandatory once you deploy, but alternatives persist — jurisdiction shopping, architecture choices, open-weight distribution. Resistance 0.55: sustained industry lobbying, inter-jurisdictional competition for AI investment, and open-source friction against licensing-style obligations. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the interval runs from soft ethics guidelines to hardened, fining, staffed-up enforcement, and the rising trajectory models that maturation rather than a static picture. All three series share one time grid ({0,3,6,9,12,15}) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. Workers and data subjects experience protection — their d sits near the beneficiary end and the regime looks like subsidy. Startups and enhancement researchers experience cost walls — high d, and the identical provisions look like toll gates. Incumbent platforms straddle: they pay the largest absolute bills yet collect the moat, placing them mid-range despite institutional power. The agenda-setter seat is the sharpest divergence: data protection authorities administer the regime as their own purpose — an institutional identity fusion in which the organization has become its function; if that identity frame broke (mandate folded into a general competition authority), their resistance to simplification would drop sharply. Same-power lateral divergence: incumbents and startups are both market actors at nominally comparable commercial standing, but constraint-specific factors — fixed-cost absorption and arbitrage-grade relocation capacity versus runway-bound constrained exit — split their experienced types despite equal nominal level.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (data subjects, workers, advocacy institutions, audit industry, incumbents) derive low d toward subsidy; declared victims (startups, enhancement researchers) derive high d toward full target, amplified by constrained exit. Incumbents' dual declaration — beneficiary in the structural array, payer as secondary role — places them intermediate rather than at either pole; the derivation handles this without an override. The agenda setter sits near symmetric administration: it neither pays the compliance stream nor consumes the protections. No directionality_overrides are authored: the beneficiary/victim declarations plus exit-option modulation produce the correct d for every seat, and the two same-power divergences (incumbent vs startup) are carried by exit_options differences the derivation already reads. Excluded seats carry no directional weight until included — their absence shapes consensus provenance, not arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — opaque algorithmic power outrunning legal categories — is attested live by courts, academic audits, and journalism from outside the beneficiary set, so founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no zombie flag, and mandatrophy_resolved is correctly left undeclared. The classification prevents mislabeling in both directions: naming the victims alone would suggest pure extraction, but the coordination function (trust infrastructure that makes high-stakes deployment possible at all) is real and independently valuable — hence not a snare. Naming the beneficiaries alone would suggest pure coordination, but audit-fee streams, incumbent moats, and bureaucratic self-expansion are extraction running through the same structure that coordinates — hence not a rope. Tangled rope is the honest center: both functions, one structure, active enforcement holding it together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel human_dignity_ai_safeguarding; which structural elements change under the sibling readings, and where exactly is the disagreement located?',
    'Comparative classification across the three reading files: diff the beneficiary/victim sets, enhancement posture, and suppression profiles; locate the disputed element as the ground of dignity (divine image vs autonomy-rationality vs constitution-independence).',
    'Under imago_dei_reading, dignity is equal prior to capability, shifting the enhancement posture toward prohibition and removing rationality-threshold exclusions from the protected set; under posthumanist_reading, the duty-bearing set expands toward synthetic persons and the human/agent boundary stops being load-bearing. Victim and beneficiary sets recompute accordingly, and the computed type can move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file instantiates the autonomy_rights_reading; the contest lives in the ground of dignity.').

omega_variable(
    rationality_ground_marginal_cases,
    'If dignity is grounded in autonomy and rationality, what protects humans at the margins of those capacities — infants, advanced dementia, severe cognitive disability — and does the regime''s uniform protection actually follow from the stated ground or from borrowed premises?',
    'Doctrinal analysis and case law tracing whether margin-case protections are derived from autonomy-premises or asserted independently of them.',
    'If borrowed, the regime''s foundation is thinner than claimed and capability-based exclusions become easier to defend, raising effective extraction on marginal groups; if derivable, the reading survives its hardest internal test and the uniform protected set stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_ground_marginal_cases, conceptual, 'Whether the reading''s stated ground supports its actual protected set.').

omega_variable(
    compliance_cost_incidence,
    'Do compliance costs fall disproportionately on small developers relative to revenue, producing an incumbent-moat effect?',
    'Post-regulation cost surveys normalized by firm size; market-concentration trends in regulated AI segments over the interval.',
    'Confirmed disproportion pushes the extraction component toward incumbent-protective rent and the computed operation toward snare-flavored behavior despite the real coordination core; rough proportionality supports the coordination framing and keeps the tangled_rope reading stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Whether the extraction component is incidence-skewed toward entrenching incumbents.').

omega_variable(
    consent_infrastructure_functionality,
    'Is the consent apparatus functional — genuine, informed, revocable choices — or predominantly theatrical: dark patterns, bundled consent, take-it-or-leave-it terms?',
    'Choice-architecture audits, revocation-rate studies, and regulator enforcement statistics on dark-pattern actions.',
    'Predominantly theatrical consent raises theater_ratio further and converts data subjects'' nominal beneficiary seat toward payer, since the protection they receive is performative while the compliance economy around it is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_infrastructure_functionality, empirical, 'Functional-versus-theatrical status of the regime''s central protective instrument.').

omega_variable(
    enhancement_permission_line,
    'Where does ''cautious enhancement permitted within rights constraints'' actually bind — which interventions pass and which fail, and is the line principled or negotiated case-by-case?',
    'Comparative approval decisions across jurisdictions over the coming decade; review-board outcome datasets.',
    'A principled line stabilizes neurotech researchers as ordinary payers with predictable costs; a negotiated line makes their exposure discretionary and drives their effective d toward full target, deepening the extraction asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_permission_line, preference, 'Principled-versus-discretionary character of the enhancement permission boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__autonomy_rights_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t3, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 3, 0.17).
narrative_ontology:measurement_basis(huma_tr_t3, observed).
narrative_ontology:measurement(huma_tr_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(huma_tr_t6, observed).
narrative_ontology:measurement(huma_tr_t9, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 9, 0.26).
narrative_ontology:measurement_basis(huma_tr_t9, observed).
narrative_ontology:measurement(huma_tr_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(huma_tr_t12, observed).
narrative_ontology:measurement(huma_tr_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(huma_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t3, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement_basis(huma_be_t3, observed).
narrative_ontology:measurement(huma_be_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement_basis(huma_be_t6, observed).
narrative_ontology:measurement(huma_be_t9, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement_basis(huma_be_t9, observed).
narrative_ontology:measurement(huma_be_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(huma_be_t12, observed).
narrative_ontology:measurement(huma_be_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(huma_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t3, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 3, 0.29).
narrative_ontology:measurement_basis(huma_su_t3, observed).
narrative_ontology:measurement(huma_su_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(huma_su_t6, observed).
narrative_ontology:measurement(huma_su_t9, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement_basis(huma_su_t9, observed).
narrative_ontology:measurement(huma_su_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement_basis(huma_su_t12, observed).
narrative_ontology:measurement(huma_su_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(huma_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__imago_dei_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__posthumanist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'human dignity in AI safeguarding' decomposes into three structurally distinct constraints — one per reading of the kernel — because the ground of dignity (divine image / autonomy-rationality / constitution-independence) changes the victim set, the enhancement posture, and the enforcement logic. Measuring dignity-protection by theological-equality observables yields different epsilon than measuring by rights-instrument observables; per the epsilon-invariance principle these are different constraints, not one constraint with a measurement parameter. This file is the autonomy_rights_reading member. Family links run through network.affects_constraints in all three files; the charter-anchored rights framework exerts structural influence on the environments the sibling readings operate in without resolving the contest among them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
