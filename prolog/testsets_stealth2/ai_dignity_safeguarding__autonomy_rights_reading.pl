% ============================================================================
% CONSTRAINT STORY: ai_dignity_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_dignity_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: ai_dignity_safeguarding__autonomy_rights_reading
 *   human_readable: AI Dignity Safeguarding — Autonomy-Rights Reading (Democratic Accountability Regime)
 *   domain: political/technological/ethical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'ai_dignity_safeguarding': the autonomy-rights reading, in which dignity
 *   is grounded in human autonomy, rationality, and rights, and safeguarding
 *   takes the form of democratic regulation — transparency mandates, labor
 *   and privacy protection, algorithmic accountability, and consent-gated
 *   openness to human enhancement. The standing arrangement under contest
 *   (and the sole referent of epsilon here) is the rights-regulatory
 *   apparatus itself: data protection and AI oversight statutes,
 *   conformity-assessment and audit requirements, platform-labor rules, and
 *   the enhancement-permission envelope bounded by informed consent and
 *   rights preservation. The reading assigns AI systems to a regulated-tool
 *   category: development proceeds, but inside accountability machinery. Its
 *   declared beneficiaries are autonomous rights-bearing citizens, the
 *   compliance-assurance profession the machinery employs, incumbents whose
 *   scale converts fixed compliance into relative advantage, and enhancement
 *   seekers served inside the consent envelope. Its declared cost-bearers are
 *   algorithmic decision subjects who still receive opaque outcomes where
 *   enforcement lags, small developers for whom compliance is a
 *   survival-level tax, and frontier enhancement labs whose protocol space
 *   the rights limits bound. Sibling readings of the same kernel are separate
 *   constraints in separate files; nothing about them enters this story's
 *   metrics.
 *
 * KEY AGENTS:
 *   - - autonomous_rights_bearers: Primary beneficiary (moderate/constrained) — the protected class the arrangement exists for; electoral leverage collectively, immobility individually
 *   - - algorithmic_decision_subjects: Primary target (powerless/trapped) — bear residual opaque-decision, displacement, and coercion harms where enforcement lags
 *   - - incumbent_ai_developers: Dual-positioned (powerful/arbitrage) — largest absolute compliance payer and principal relative gainer
 *   - - small_ai_developers: Secondary target (moderate/mobile) — compliance as survival-level tax
 *   - - enhancement_frontier_labs: Bounded innovator (powerful/arbitrage) — protocol space limited by consent and rights gates
 *   - - algorithmic_accountability_regulators: Agenda setter (institutional/constrained) — administers, fines, certifies; budget grows with mandate
 *   - - accountability_compliance_industry: Secondary beneficiary (organized/mobile) — sells the assurance the machinery requires
 *   - - enhancement_seeking_individuals: Conditional beneficiary (moderate/constrained) — served inside the consent envelope, unserved outside it
 *   - - civil_society_watchdogs: Analytical observer (organized/analytical) — audits, publishes, litigates; no enforcement power
 *   - - extraterritorial_algorithmic_populations: Excluded voice (powerless/trapped) — governed by the regime's products, outside its reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_dignity_safeguarding__autonomy_rights_reading, 0.38).
domain_priors:suppression_score(ai_dignity_safeguarding__autonomy_rights_reading, 0.42).
domain_priors:theater_ratio(ai_dignity_safeguarding__autonomy_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_dignity_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(ai_dignity_safeguarding__autonomy_rights_reading, "AI Dignity Safeguarding — Autonomy-Rights Reading (Democratic Accountability Regime)").
narrative_ontology:topic_domain(ai_dignity_safeguarding__autonomy_rights_reading, "political/technological/ethical").

domain_priors:requires_active_enforcement(ai_dignity_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_dignity_safeguarding__autonomy_rights_reading, '32f69e8b-981d-4f3d-8d31-e3d3ccf86875').
narrative_ontology:cs_kernel_codification('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', distributed).
narrative_ontology:cs_authority_grounding('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', distributed).
narrative_ontology:cs_reading_relation('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', ai_dignity_safeguarding__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', ai_dignity_safeguarding__posthuman_continuity_reading, influences).
narrative_ontology:cs_axiom('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', foundational, dignity_ground_is_autonomy_rationality_rights).
narrative_ontology:cs_axiom_status(dignity_ground_is_autonomy_rationality_rights, holdable).
narrative_ontology:cs_axiom_grounding('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', dignity_ground_is_autonomy_rationality_rights, deontological).
narrative_ontology:cs_axiom('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', foundational, enhancement_permitted_within_rights_limits).
narrative_ontology:cs_axiom_status(enhancement_permitted_within_rights_limits, holdable).
narrative_ontology:cs_axiom_grounding('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', enhancement_permitted_within_rights_limits, instrumental).
narrative_ontology:cs_reference_frame('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', autonomous_agent_rights_baseline).
narrative_ontology:cs_drift_state('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', contemporary_foundation_model_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32f69e8b-981d-4f3d-8d31-e3d3ccf86875', '').
narrative_ontology:cs_kernel_id(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rights_bearers).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_developers).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, accountability_compliance_industry).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_seeking_individuals).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, small_ai_developers).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_frontier_labs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_accountability_regulators).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_developers).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_seeking_individuals).
narrative_ontology:constraint_vindicates(ai_dignity_safeguarding__autonomy_rights_reading, informed_consent_principle).
narrative_ontology:constraint_vindicates(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_transparency_doctrine).
narrative_ontology:constraint_vindicates(ai_dignity_safeguarding__autonomy_rights_reading, meaningful_human_oversight_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens, consumers, patients, and data subjects living under the arrangement. They vote for the legislatures that write the rules, click through the consent screens, and are scored, hired, treated, and managed by automated systems. What flows to them: disclosure of how decisions about them are made, channels to contest outcomes, and protections for their data and jobs. What flows from them: taxes funding the enforcement bodies and, indirectly, higher prices where compliance costs pass through. Leaving is not realistic — the algorithmic services they would exit are the same ones their work, healthcare, and civic life run on.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rights_bearers, beneficiary,
    moderate, generational, constrained, national).

% People on the receiving end of automated decisions — welfare applicants scored by opaque models, warehouse workers paced by algorithms, patients triaged by systems they cannot inspect, gig workers deactivated by automated review. The arrangement promises them explanation and recourse; where enforcement lags or exemptions apply, they still bear the unexplained decision, the pace, the deactivation. They have the least ability of anyone in the story to move away from the systems that classify them.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_decision_subjects, payer,
    powerless, biographical, trapped, national).

% Large platform and foundation-model companies. They fund the bulk of compliance: documentation, audits, transparency reports, legal teams. They also gain relative ground: fixed compliance overhead weighs less on them than on smaller rivals, and rules they helped draft tend to match architectures they already run. Their exit is portfolio-shaped rather than physical — they lobby, litigate, shift product lines, and locate training runs where rules are loosest.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_developers, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_developers, beneficiary).

% Startups and independent labs building models and applications. Every hour spent on conformity paperwork is an hour not spent on product; audit fees and counsel costs are a larger share of runway than for incumbents. Some relocate to permissive jurisdictions or ship only where rules are light; many simply absorb the cost as the price of market access.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, small_ai_developers, payer,
    moderate, biographical, mobile, global).

% Neurotechnology, genetic, and cognitive-enhancement ventures. The arrangement lets them operate but draws lines: consent must be genuine, trials must respect rights, some interventions stay off the table entirely. They spend on ethics review and trial design to stay inside the lines, and some route ambitious protocols to jurisdictions with fewer limits.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_frontier_labs, payer,
    powerful, generational, arbitrage, global).

% Data protection authorities, AI oversight offices, and sectoral agencies. They write guidance, investigate complaints, penalize violators, and certify high-risk systems. Each new mandate enlarges their staff and budget. Their personnel circulate to and from the industry they oversee. They cannot abandon the mandate short of legislative repeal, but they choose enforcement priorities.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_accountability_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_accountability_regulators, beneficiary).

% Audit firms, conformity-assessment bodies, ethics consultancies, and the in-house compliance profession. The arrangement created their market: impact assessments, bias audits, documentation suites, certification marks. They sell assurance to developers and reassurance to regulators. Their livelihood depends on the rules staying detailed enough to require experts.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, accountability_compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% People who want sharper memory, mood, focus, or longer healthspan. Consent-gated enhancement is available to them where providers comply; where a technique they want falls outside the rights-preserving envelope — or costs what only the wealthy can pay — they go without, self-source, or travel.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_seeking_individuals, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_seeking_individuals, payer).

% NGOs, digital-rights organizations, worker centers, and academic audit labs. They test systems, publish failures, file complaints, and push for stronger rules. They hold no enforcement power; their influence runs through publicity, litigation support, and agenda-setting.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, civil_society_watchdogs, observer,
    organized, generational, analytical, global).

% Residents of non-democratic jurisdictions who are scored, managed, and surveilled by systems built and marketed under this arrangement, but who live outside its enforcement reach. They would object to export-grade opacity and to enhancement supply chains that route around consent standards; they have no seat in the consultations, no recourse channel, and no realistic exit from the systems their governments and employers adopt.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, extraterritorial_algorithmic_populations, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_developers).
narrative_ontology:fixing_cost_class(ai_dignity_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem no individual can solve alone: against deployers of algorithmic systems, single data subjects and workers have no leverage to demand explanation, contest outcomes, or set labor terms; the arrangement standardizes disclosure, recourse, consent, and oversight once, centrally, for whole markets, and coordinates enhancement innovation against coercion risk through trial and consent requirements.
% TRANSFER_FUNCTION: Moves compliance cost — documentation, audit fees, conformity assessment, legal overhead — from the general public (who would otherwise absorb unchecked algorithmic harm) to developers and deployers; moves assurance revenue from regulated firms to auditors, certification bodies, and enforcement agencies; moves decision-relevant information from deployers to the people their systems act on.
% ABSENT_VOICES: Residents of non-democratic jurisdictions subject to systems built under this arrangement but beyond its enforcement (authored as the excluded stakeholder extraterritorial_algorithmic_populations); future persons whose enhancement baseline is being set by today's permission envelope; gig and warehouse workers in sectors where union presence is too thin to carry their objections into consultation; and — constitutively for this reading — artificial systems themselves, which hold no seat because dignity attaches only to autonomous rational agents; their interests enter only as risks to persons.
% DISAPPEARANCE_RATIONALE: Overnight repeal would not return the world to its pre-regulatory state: deployed systems, compliance professions, and case law persist, but new deployment would reorganize around deployer preference — opacity re-expanding wherever disclosure was the only thing forcing it, algorithmic management repricing labor control, enhancement markets expanding past consent gates within quarters. Enforcement agencies, audit firms, and rights litigators would lose their object; decision subjects would lose the recourse channel that is currently the difference between an appealable error and a silent one.
% FOUNDING_PROBLEM: Mid-2010s algorithmic turn: opaque scoring entered welfare, credit, hiring, and policing; platforms began managing labor through metrics and automated discipline; data extraction outpaced any individual's capacity to bargain. The problem the arrangement was built to solve: how machine-made decisions about persons can be governed so that autonomy, rational agency, and rights survive automation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: labor federations and worker centers document algorithmic-management grievances; investigative journalism and academic audits repeatedly surface opaque or discriminatory scoring in welfare, credit, and hiring; discrimination and data-protection litigation dockets grow yearly. None of these seats collects from the arrangement's existence — their testimony is adverse-interest evidence that the founding problem persists.
narrative_ontology:disappearance_verdict(ai_dignity_safeguarding__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_dignity_safeguarding__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_dignity_safeguarding__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_dignity_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_dignity_safeguarding__autonomy_rights_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).
:- end_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness 0.38: the arrangement transfers real, recurring costs — documentation, audit fees, legal overhead, forgone protocol space — but its designed function is protective, and much of what it moves is information and recourse rather than rent; low-to-moderate, rising. Suppression 0.42: fines, market exclusions, and prohibitions on non-consensual intervention are genuine coercion, bounded by democratic legitimation and judicial review; the suppression_requirement series is authored deliberately because the story's enforcement picture is NOT static — inspection capacity, penalty scales, and conformity-assessment mandates were built up across the interval, and that build-up is the dynamic being traced. Theater_ratio 0.40: a substantial share of activity is ethics performance — impact assessments written to pass, bias audits commissioned for the certificate, principles pages decoupled from deployment — alongside real disclosure and real penalties. Accessibility_collapse 0.30: alternatives remain workable — offshore training runs, open-weight releases, permissive jurisdictions, informal enhancement markets — so understanding the rules does not close the option set. Resistance 0.55: sustained lobbying, jurisdiction shopping, and compliance-minimization meet every tightening. All three series share one grid (points 0,2,4,6,8,10, mapping approximately to 2016-2026); there is no oscillation — drift is monotonic, driven by enforcement maturation and compliance-industry growth rather than crisis cycles. Coalition note: the trapped decision-subject seat is not permanently powerless — unionization and collective litigation are the observed counterweight, and their strength modulates that seat's effective position.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. Algorithmic decision subjects experience the arrangement as a promise that outruns delivery: explanation and recourse exist on paper, and where inspectors are thin they still absorb the opaque score, the pacing algorithm, the automated deactivation — from that seat the machinery looks like cover. Small developers experience a cost wall that decides survival. Incumbents occupy both sides at once: they pay the largest absolute compliance bills and harvest the largest relative advantage, so the same statute reads as burden in the earnings call and as moat in strategy. The compliance profession experiences a market. The regulator experiences mandate growth. Civil-society watchdogs experience partial victory — real tools, insufficient reach. Nothing in the authored claim adjudicates among these; the engine derives each seat's classification from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: autonomous rights bearers (protected, electorally leveraged but individually immobile), the compliance industry (collects the fees the machinery generates), enhancement seekers inside the consent envelope. Victim declarations drive high d: decision subjects sit nearest the full-target end — trapped exit, powerless, bearing the residual harms the machinery exists to prevent; small developers bear disproportionate cost with mobile exit softening their position slightly; frontier labs bear bounded protocol space with arbitrage exit softening further. Incumbents derive mid-range from their dual declaration — listed among beneficiaries for the moat effect yet paying the largest absolute compliance costs; the derivation nets them below symmetric but above pure beneficiary. No directionality_overrides are authored: the override surface keys on power atoms, and at every contested level ('powerful', 'moderate') the seats split in opposite directions, so any override would misassign one side. Spatial scope amplifies effective extraction modestly for the global-scope payer seats, where verifying compliance across jurisdictions is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared and none is due: the founding problem — governing machine-made decisions so autonomy and rights survive automation — is corroborated as live from outside the beneficiary set (union grievances over algorithmic management, investigative and academic audits of opaque scoring, active litigation dockets). The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag. The live risk is not obsolescence but accumulation: theater_ratio and base_extractiveness rise together across the interval, the classic signature of rent-seeking layering onto coordination. If a future interval showed the founding problem solved while the apparatus kept growing, the degraded-inertial hypothesis (prohibitive fixing, captured or diffuse gains) becomes the candidate to test; this story does not assert it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_grounding_kernel_ambiguity,
    'This constraint is one reading of the ai_dignity_safeguarding kernel. Would instantiating the imago_dei_reading or the posthuman_continuity_reading instead produce a structurally different arrangement — different victim sets, different permission envelopes — from the same underlying commitment?',
    'Not resolvable by data alone: the readings are rival groundings of one commitment. Resolution proceeds by observing which grounding a polity institutionalizes — compare jurisdictions where theological dignity language enters binding law against those where rights language exhausts the statutory frame.',
    'Under the imago_dei_reading the enhancement envelope contracts toward prohibition and the victim set adds subjects of nature-transgressing intervention; under the posthuman_continuity_reading consent-gating loosens toward enablement and the coercive-enhancement victim category thins. This story''s epsilon (0.38) is indexed to the autonomy-rights instantiation only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_grounding_kernel_ambiguity, conceptual, 'Committer-frame omega: which grounding of the dignity kernel a polity adopts determines the structural shape of the safeguarding arrangement.').

omega_variable(
    protection_vs_compliance_artifact_gap,
    'Does the accountability machinery actually reduce opaque-algorithm harm to decision subjects, or does it primarily generate compliance artifacts that leave frontline exposure unchanged?',
    'Longitudinal outcome studies: complaint-resolution rates, measured algorithmic-opacity incidence in audited sectors before and after conformity mandates, worker deactivation-appeal success rates.',
    'If artifacts dominate, the arrangement''s coordination function is thinner than declared and effective extraction on decision subjects is higher than the base measure suggests — pushing computed classifications toward the extractive end for the trapped seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_compliance_artifact_gap, empirical, 'Whether protection outcomes track compliance output.').

omega_variable(
    compliance_moat_net_effect,
    'Does fixed-compliance scaling net-help or net-hurt small developers once market access, trust signals, and certified-vendor procurement preferences are counted?',
    'Entry/exit rates and funding flows for AI startups in high-enforcement versus permissive jurisdictions; procurement data on certification preferences in public and enterprise buying.',
    'If the moat dominates, incumbent capture of the arrangement is confirmed and the beneficiary declaration for incumbents strengthens; if access effects dominate, their directional position sits nearer symmetric than the moat reading implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_moat_net_effect, empirical, 'Net direction of the compliance-cost asymmetry for small developers.').

omega_variable(
    enhancement_consent_boundary_location,
    'Where does ''consent-based and rights-preserving'' actually end — workplace neuro-monitoring under soft pressure, consumer neurotech terms of service, heritable-editing moratoria, over-the-counter cognitive enhancers?',
    'Case-by-case adjudication as regulators and courts confront specific techniques; comparative analysis across jurisdictions that draw the line differently.',
    'A narrower boundary shrinks the permission envelope, raising effective extraction on frontier labs and enhancement seekers; a wider one admits coercion-risk categories back into lawful practice, thickening the decision-subject victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_consent_boundary_location, conceptual, 'The consent boundary is drawn case-by-case; its location is unstable.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will enforcement capacity keep ratcheting up with each scandal and statute, or decay under industry pressure, budget cycles, and revolving-door personnel flow?',
    'Track inspector headcount, penalty frequency and magnitude, and conformity-assessment throughput across the next interval.',
    'Continued ratchet pushes suppression and extraction upward together, hardening conditions for the trapped seat; decay leaves the machinery increasingly theatrical, with performative compliance outgrowing functional share.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Direction of the enforcement build-up traced by the suppression_requirement series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_dignity_safeguarding__autonomy_rights_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_d_tr_t0, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ai_d_tr_t0, observed).
narrative_ontology:measurement(ai_d_tr_t2, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(ai_d_tr_t2, observed).
narrative_ontology:measurement(ai_d_tr_t4, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(ai_d_tr_t4, observed).
narrative_ontology:measurement(ai_d_tr_t6, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(ai_d_tr_t6, observed).
narrative_ontology:measurement(ai_d_tr_t8, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(ai_d_tr_t8, observed).
narrative_ontology:measurement(ai_d_tr_t10, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(ai_d_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(ai_d_be_t0, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(ai_d_be_t0, observed).
narrative_ontology:measurement(ai_d_be_t2, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 2, 0.26).
narrative_ontology:measurement_basis(ai_d_be_t2, observed).
narrative_ontology:measurement(ai_d_be_t4, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement_basis(ai_d_be_t4, observed).
narrative_ontology:measurement(ai_d_be_t6, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement_basis(ai_d_be_t6, observed).
narrative_ontology:measurement(ai_d_be_t8, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement_basis(ai_d_be_t8, observed).
narrative_ontology:measurement(ai_d_be_t10, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(ai_d_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_d_su_t0, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(ai_d_su_t0, observed).
narrative_ontology:measurement(ai_d_su_t2, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 2, 0.29).
narrative_ontology:measurement_basis(ai_d_su_t2, observed).
narrative_ontology:measurement(ai_d_su_t4, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement_basis(ai_d_su_t4, observed).
narrative_ontology:measurement(ai_d_su_t6, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement_basis(ai_d_su_t6, observed).
narrative_ontology:measurement(ai_d_su_t8, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(ai_d_su_t8, observed).
narrative_ontology:measurement(ai_d_su_t10, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(ai_d_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_dignity_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_safeguarding__imago_dei_reading).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_safeguarding__posthuman_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'AI dignity safeguarding' decomposes into three structurally distinct constraints — one per reading of the kernel — because the readings assign different victim sets and different enhancement-permission envelopes, hence different epsilon values. This file is the autonomy-rights member: epsilon 0.38 over the standing rights-regulatory arrangement, assessed by this reading's own lights. The imago_dei member carries a prohibition-leaning envelope and a theology-indexed victim set; the posthuman_continuity member carries an enablement-leaning envelope and a thinned coercive-enhancement victim category. Upstream/downstream: this reading is the most institutionally established (instantiated in binding statute), so its edges run outward — its consent-and-rights gating changes the operating environment of the posthuman reading without foreclosing it, and its rights vocabulary pressures the theological reading toward translation without eliminating it. Each family member links to the others via network.affects_constraints; orphan stories would break contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
