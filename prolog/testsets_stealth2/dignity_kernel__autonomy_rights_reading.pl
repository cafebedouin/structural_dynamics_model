% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Reading of Dignity in Technology Governance
 *   domain: theological ethics/technology governance/philosophical anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested dignity kernel: the
 *   autonomy-rights reading, on which a person's worth is grounded in
 *   autonomous rational agency and secured through enforceable rights rather
 *   than in divine image. The standing arrangement under assessment is that
 *   reading as institutionalized in technology governance: transparency,
 *   accountability, privacy and labor floors for AI systems, and enhancement
 *   permitted within rights limits. Stated assumptions: the epsilon referent
 *   is this standing arrangement assessed by the reading's own lights (not
 *   the imago-dei or posthumanist alternatives, which are separate stories
 *   with their own epsilon values); the claimed type and the metrics are
 *   authored independently, the claim recording my structural judgment that a
 *   genuine rights-coordination core carries a real extraction layer, the
 *   metrics recording the arrangement's measured operation. KEY AGENTS (by
 *   structural relationship): - rights_regulators_and_courts: agenda setter
 *   (institutional/analytical) - administers and interprets the settlement -
 *   ai_developers_operators: dual-positioned payer-beneficiary
 *   (institutional/arbitrage) - bears the largest absolute compliance costs,
 *   recoups trust and moat advantages - small_ai_developers: payer
 *   (moderate/constrained) - fixed compliance costs are regressive against
 *   small budgets - data_subjects: primary beneficiary
 *   (powerless/constrained) - protected class whose protection depends on
 *   enforcement they do not control - algorithmically_managed_workers:
 *   beneficiary (moderate/constrained) - labor and surveillance floors -
 *   compliance_audit_industry: beneficiary and rent collector
 *   (organized/mobile) - fee income scales with mandated complexity -
 *   cognitively_disabled_persons: payer (powerless/trapped) - bear the
 *   boundary cost of capacity-keyed moral consideration -
 *   religious_institutions: excluded (institutional/identity_locked) -
 *   grounding carries no adjudicating weight inside the framework -
 *   enhancement_access_seekers: beneficiary-payer hybrid (organized/mobile) -
 *   lawful lane kept open, gated by rights limits -
 *   philosophical_bioethics_community: analytical observer - supplies the
 *   arguments all sides cite.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.44).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.38).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy-Rights Reading of Dignity in Technology Governance").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological ethics/technology governance/philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, 'dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b').
narrative_ontology:cs_kernel_codification('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', distributed).
narrative_ontology:cs_authority_grounding('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', expertise).
narrative_ontology:cs_interpretation_layer_present('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b').
narrative_ontology:cs_reading_relation('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', dignity_kernel__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', dignity_kernel__posthumanist_reading, influences).
narrative_ontology:cs_axiom('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', foundational, dignity_ground_is_rational_autonomy_not_divine_image).
narrative_ontology:cs_axiom_status(dignity_ground_is_rational_autonomy_not_divine_image, holdable).
narrative_ontology:cs_axiom_grounding('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', dignity_ground_is_rational_autonomy_not_divine_image, deontological).
narrative_ontology:cs_axiom('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', secondary, enhancement_lawful_within_rights_limits).
narrative_ontology:cs_axiom_status(enhancement_lawful_within_rights_limits, holdable).
narrative_ontology:cs_axiom_grounding('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', enhancement_lawful_within_rights_limits, instrumental).
narrative_ontology:cs_reference_frame('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', autonomy_rights_settlement).
narrative_ontology:cs_drift_state('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', contemporary_ai_governance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc05fdbb-9d97-4fa1-8cf5-c98c55093e8b', '').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, data_subjects).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, enhancement_access_seekers).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, compliance_audit_industry).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, cognitively_disabled_persons).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, small_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, ai_developers_operators).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, ai_developers_operators).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, enhancement_access_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the rules requiring AI systems to disclose automated decisions, protect worker and consumer data, and pass accountability review before deployment. Constitutional courts and data-protection authorities decide which enhancement practices and automated-management practices are lawful. Their position is interpretive rather than escapable: they can revise doctrine but cannot step outside the legal order they administer.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, rights_regulators_and_courts, agenda_setter,
    institutional, generational, analytical, continental).

% Build and operate the AI systems subject to transparency, accountability, privacy, and labor rules. They fund compliance teams, external audits, and documentation, and bear the largest absolute costs of the arrangement. They also receive the public trust and market legitimacy those rules create, and their scale lets them absorb fixed compliance costs that smaller rivals cannot, so some quietly defend obligations they publicly criticize. Relocating development to permissive jurisdictions is possible but disruptive.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, ai_developers_operators, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, ai_developers_operators, beneficiary).

% Face the same disclosure, documentation, and audit duties as large firms without comparable legal or compliance staff. Fixed compliance costs weigh heavily against small budgets; some delay launches, narrow product scope, or sell to larger firms rather than carry the burden. Pivoting to unregulated niches remains possible but shrinks as the rules extend.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, small_ai_developers, payer,
    moderate, biographical, constrained, global).

% Live under automated decisions about credit, housing, hiring, and essential services. The arrangement promises notice, explanation, correction rights, and limits on data use. Individually they hold little leverage and cannot practically exit digital life; their protection depends on enforcement they do not control.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, data_subjects, beneficiary,
    powerless, biographical, constrained, global).

% Work under scheduling, evaluation, and dispatch algorithms. The rules promise human review of consequential decisions, wage and hour floors, and limits on workplace surveillance. Unionization is uneven across sectors and countries; leaving a platform job is possible but costly, and protections vary widely by jurisdiction.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers, beneficiary,
    moderate, biographical, constrained, global).

% Sells the assessments, audits, documentation templates, and certifications the rules require. Fee income scales with the number and complexity of mandated checks. Staff and clients move freely between jurisdictions, and the trade expands wherever new obligations appear.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, compliance_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% People with severe intellectual or cognitive disabilities live under guardianship, substituted decision-making, and capacity assessments that condition how much weight their own choices receive. Because the prevailing framework ties moral consideration closely to rational autonomy, their claims are routinely mediated by others. They cannot exit the category; advocacy runs through families and allied movements.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, cognitively_disabled_persons, payer,
    powerless, biographical, trapped, global).

% Churches and theological traditions hold that human worth rests on divine image rather than on capacities. In the forums where technology rules are written, that grounding carries no adjudicating weight; their contributions are heard only when translated into rights language. Abandoning the theological ground is not an option open to them.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, religious_institutions, excluded,
    institutional, civilizational, identity_locked, global).

% Communities seeking cognitive, biological, and longevity enhancement operate inside a lane the rules keep open: enhancement is permitted where it respects consent, safety, and fairness requirements, and blocked or delayed elsewhere. Approval processes, eligibility criteria, and cross-border restrictions shape what they can pursue, and medical travel to permissive jurisdictions is a live option.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, enhancement_access_seekers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, enhancement_access_seekers, payer).

% Scholars and advisory commissions analyze who counts as a bearer of worth, how capacity relates to status, and where enhancement limits should sit. They supply the arguments every side cites and hold no material stake in which account prevails.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, philosophical_bioethics_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__autonomy_rights_reading, compliance_audit_industry).
narrative_ontology:fixing_cost_class(dignity_kernel__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for developing and deploying AI and enhancement technologies without sacrificing the people those technologies act on: transparency about automated decisions, accountability for outcomes, floors for privacy and labor protection, and a lawful lane for enhancement bounded by rights. Innovation and protection proceed together instead of as an unregulated race or a moratorium.
% TRANSFER_FUNCTION: Moves disclosure, documentation, and compliance effort from AI developers and operators toward data subjects, workers, and oversight bodies; moves approval authority over enhancement from open market allocation into rights-bounded review; and displaces theological institutions' historic authority over worth-questions in favor of courts and expert bodies applying rights doctrine.
% ABSENT_VOICES: Religious institutions are present in public debate but excluded from adjudicative weight inside the framework: their grounding cannot be restated in rights terms, so their objection never enters the room where rules are finalized. Severely cognitively disabled people are spoken about through guardians and advocates rather than heard directly. Artificial or radically enhanced minds have no seat at all; the claim that the human boundary itself is arbitrary is voiced only by human proxies.
% DISAPPEARANCE_RATIONALE: Constitutional clauses, data-protection statutes, research-ethics committees, and AI accountability laws all cite worth-grounded-in-autonomy-and-rights as their warrant. Overnight removal would strip the operative justification from GDPR-style regimes, unsettle capacity-and-consent doctrine across medicine and research, and reopen every enhancement and automated-decision question at once; courts, firms, and legislatures would have to rebuild the settlement from scratch.
% FOUNDING_PROBLEM: After mechanized atrocity showed that worth assigned by hierarchy and race licenses extermination, pluralistic societies needed a ground for human worth that depended on no particular theology or metaphysics and could bind believers and unbelievers alike. Autonomous rational agency secured by enforceable rights supplied that common ground; it was later extended to govern technologies that act on persons.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: UN treaty bodies and national constitutional courts re-invoke the founding problem each time a new technology arrives; disability-rights movements and the CRPD committee attest both that the problem is live and that the framework's capacity-keying mistreats them; industry submissions to AI-legislation consultations attest the compliance burden is real; theological ethicists attest the displacement of their grounding. No party attests the founding problem is closed.
narrative_ontology:disappearance_verdict(dignity_kernel__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__autonomy_rights_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).
:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.44 because the arrangement's dominant flow is protective (disclosure, correction rights, labor and privacy floors) while a real secondary layer transfers value: regressive compliance incidence against small developers, audit and certification fees scaling with mandated complexity, and the boundary cost borne by people whose moral consideration is keyed to rational capacity. Suppression is 0.38: enforcement machinery is mature and procedural gating is real (rival groundings must translate into rights language to gain purchase), but nothing approaching coercion suppresses the sibling readings, which remain legally and intellectually live. Theater is 0.42 and rising: principles documents, toothless ethics boards, and unread transparency reports are endemic in AI governance, yet genuine audits and enforceable protections coexist with the performance. Accessibility collapse is 0.40: alternatives have not collapsed; the imago-dei and posthumanist readings remain live and light-touch jurisdictions persist, though official fora converge heavily on rights framing. Resistance is 0.55: industry lobbying, religious objection, transhumanist dissent, jurisdictional arbitrage, and disability-rights critique from inside the rights tradition. The three temporal series run on one shared grid (1948, 1965, 1980, 1995, 2010, 2025) so every metric is authored at every examined point; all trajectories are monotonic rather than cyclical, reflecting accumulation rather than oscillation. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. The suppression_requirement series is included because enforcement capacity genuinely changed over the interval, from a thin declaration regime to a mature regulatory apparatus; the scalar suppression (0.38) is the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the regulator's position the settlement is a working constitutional achievement: predictable rules, enforceable floors, a lawful enhancement lane. From the data subject's position it is a promise whose delivery depends on enforcement they cannot trigger individually. From the small developer's position the same structure is a fixed cost wall that larger rivals clear easily. The large developer occupies a paradox seat: heaviest absolute payer, yet partial recouper through trust legitimacy and cost-raising effects on smaller rivals, which is why some firms defend obligations they publicly resist. The cognitively disabled person's seat is the sharpest divergence: the framework that promises universal worth delivers, for them, a status mediated by capacity assessments. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (data_subjects, algorithmically_managed_workers, enhancement_access_seekers, compliance_audit_industry) drive low directionality for those seats; victim declarations (cognitively_disabled_persons, small_ai_developers) drive high directionality. The large developers are deliberately left off both flat arrays: their net position is genuinely mixed (largest absolute payer, partial recouper through moat and trust effects), and their dual position is carried on the stakeholder surface instead of being forced into a single declaration. No directionality overrides are used: the derivation chain resolves the seats correctly from declarations plus exit options, and the override mechanism is keyed by power atom rather than agent, so any override would collide across same-power seats with opposed positions (for example, powerless covers both data_subjects, who sit beneficiary-side, and cognitively_disabled_persons, who sit target-side).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: technologies that act on persons have multiplied since the settlement was built, and the protection problem it was built for is more acute, not less. The R5 mismatch consumer therefore finds status=live crossed with verdict=world_rearranges, which raises no zombie flag, and mandatrophy is not resolved. The live risk is not obsolescence but Goodhart drift: the theater_ratio series climbs monotonically (0.15 to 0.42) as principles documents and symbolic ethics boards proliferate alongside real enforcement. If performative activity fully substitutes for functional enforcement, the arrangement drifts toward a piton profile, a settlement administered theatrically while its protective core atrophies. The classification prevents mislabeling in both directions: reading the compliance layer as pure extraction would erase the genuine coordination achievement; reading the rights rhetoric at face value would erase the regressive cost incidence and the capacity-keyed exclusion that the payer seats actually bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_grounding_disagreement,
    'Which grounding axiom should confer moral status: rational autonomy and rights (this reading), inviolable divine image prior to capability (imago-dei reading), or no fixed human limit at all (posthumanist reading)?',
    'No argument alone settles it; resolution arrives through which reading''s institutional carriers win successive governance decisions. Track adoption patterns in AI regulation, bioethics body rulings, and constitutional interpretation over successive cycles.',
    'Sibling adoption reshuffles the victim set structurally: an imago-dei settlement removes the capacity-keyed payer seat (status no longer keyed to rationality) and tightens enhancement policy; a posthumanist settlement dissolves the human boundary and weakens the rights floor protecting current persons against more capable systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_grounding_disagreement, conceptual, 'Committer structure: this constraint is one reading of dignity_kernel; the location of the disagreement is the grounding axiom.').

omega_variable(
    capacity_keying_fixability,
    'Is the cost borne by cognitively disabled persons intrinsic to grounding dignity in rational autonomy, or removable through two-tier reform that pairs a flat baseline status with capacity-tiered permissions?',
    'Compare jurisdictions implementing supported-decision-making reforms (CRPD-style) against those retaining substituted-judgment and capacity-assessment regimes: measure outcomes in guardianship scope, consent validity, and service prioritization.',
    'If the cost is reformable, the payer seat shrinks and the arrangement drifts toward a rope profile; if intrinsic to the grounding axiom, the tangled_rope classification holds and pressure toward a flat-status grounding grows from inside the rights tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_keying_fixability, empirical, 'Whether the rationality-keying exclusion is a repairable implementation defect or a structural consequence of the grounding axiom.').

omega_variable(
    compliance_rent_share,
    'What fraction of the compliance cost borne by AI developers is protection value delivered to data subjects and workers, versus rent captured by the audit and certification industry and cost-raising absorbed strategically by incumbents?',
    'Audit-market studies, compliance cost benchmarks across firm sizes, and analysis of which firms lobby for which obligation levels; incumbent support for heavier regimes is diagnostic of cost-raising motives.',
    'A high rent share pushes the commercial layer''s computed classification toward snare-flavored seats for small developers and strengthens the case for simplification remedies; a low share supports reading the compliance burden as the ordinary price of the coordination good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_rent_share, empirical, 'Decomposition of the compliance layer into protection value versus captured rent.').

omega_variable(
    ethics_theater_trajectory,
    'Is the growing share of performative activity (principles documents, advisory boards without authority, unread transparency reports) load-bearing legitimation that supports the real enforcement core, or parasitic substitution that is eroding it?',
    'Compare substantive protection outcomes of theater-heavy versus theater-light organizations at equal regulatory exposure over time; track whether enforcement actions rise or fall as symbolic output rises.',
    'Parasitic substitution predicts the theater_ratio continuing to climb past 0.5 and eventual piton drift, with the settlement administered theatrically while its protective core atrophies; load-bearing legitimation predicts a plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethics_theater_trajectory, empirical, 'Whether measured theater is functional or corrosive for the arrangement''s core.').

omega_variable(
    enhancement_gate_incidence,
    'Does the rights-limit gate on enhancement bind in ways that protect persons, or does it mainly filter access by wealth and jurisdiction while adverse activity migrates to permissive venues?',
    'Cross-jurisdiction data on who actually accesses cognitive, biological, and longevity enhancement, at what price, and with what adverse-event rates inside versus outside the gated channels.',
    'If the gate chiefly filters by wealth, a hidden payer seat of priced-out seekers exists, the enhancement-access beneficiary seat is thinner than declared, and the arrangement''s benefit distribution is more stratified than its rhetoric admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enhancement_gate_incidence, empirical, 'Distributional incidence of the enhancement gate the reading keeps open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dk_autonomy_rights_tr_t1948, dignity_kernel__autonomy_rights_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(dk_autonomy_rights_tr_t1965, dignity_kernel__autonomy_rights_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(dk_autonomy_rights_tr_t1980, dignity_kernel__autonomy_rights_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(dk_autonomy_rights_tr_t1995, dignity_kernel__autonomy_rights_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(dk_autonomy_rights_tr_t2010, dignity_kernel__autonomy_rights_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(dk_autonomy_rights_tr_t2025, dignity_kernel__autonomy_rights_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(dk_autonomy_rights_be_t1948, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(dk_autonomy_rights_be_t1965, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1965, 0.26).
narrative_ontology:measurement(dk_autonomy_rights_be_t1980, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1980, 0.31).
narrative_ontology:measurement(dk_autonomy_rights_be_t1995, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(dk_autonomy_rights_be_t2010, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(dk_autonomy_rights_be_t2025, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2025, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(dk_autonomy_rights_su_t1948, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(dk_autonomy_rights_su_t1965, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1965, 0.21).
narrative_ontology:measurement(dk_autonomy_rights_su_t1980, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1980, 0.27).
narrative_ontology:measurement(dk_autonomy_rights_su_t1995, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement(dk_autonomy_rights_su_t2010, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(dk_autonomy_rights_su_t2025, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, identity_coordination).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'human dignity' decomposes into at least three structurally distinct constraints: this autonomy-rights reading, the imago-dei reading, and the posthumanist reading. Each has its own epsilon, victim set, and enhancement stance; they form a constraint family linked through affects_constraints. Historical flow runs imago-dei (upstream, medieval through early modern) to autonomy-rights (midstream, Enlightenment through the UDHR, which borrowed the rights form while replacing the theological grounding) to posthumanist (downstream challenger to both). This story's epsilon (0.44) prices the compliance-layer extraction and the capacity-keyed exclusion; an imago-dei story would price enforcement-of-doctrinal-orthodoxy costs and would dissolve the cognitively-disabled payer seat; a posthumanist story would price deregulatory exposure and the weakened rights floor. Sibling stories must mirror this note and link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
