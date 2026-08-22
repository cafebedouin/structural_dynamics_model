% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Licensing Mandate: Rent-Seeking Suppression Reading
 *   domain: economic/regulatory/labor
 *
 * SUMMARY:
 *   Statutory credential requirements for professional practice (licensing
 *   laws for attorneys, physicians, electricians, etc.) are presented by
 *   incumbent practitioners and regulatory agencies as consumer protection
 *   mechanisms—minimum competence standards that prevent harm. This reading
 *   frames the same statutes as rent-seeking suppression: the primary
 *   structural function is restricting labor supply to maintain elevated
 *   professional income, with the public safety narrative serving as cover.
 *   The examination barriers, credential stacking, and continuous credential
 *   inflation are mechanisms to restrict supply and extract rents from
 *   consumers and aspiring entrants. The founding problem (information
 *   asymmetry in the 1800s) is substantially solved; the statutes persist as
 *   income-protection devices.
 *
 * KEY AGENTS:
 *   - Incumbent practitioners: established professionals who benefit from restricted supply and elevated income; set examination standards through professional associations and regulatory capture; organized power to defend barriers
 *   - Potential labor market entrants: blocked from practice by artificial credential barriers; trapped within the jurisdiction (occupational licensing is state-regulated); powerless to change standards
 *   - Consumers via price inflation: pay higher fees due to restricted supply; need the service (most are non-optional) and cannot substitute unlicensed alternatives; powerless to exit
 *   - Licensing board administrators: enforce the examination and credential barriers on behalf of incumbent practitioners; nominally neutral but structurally captured; gate-keepers of supply restriction
 *   - Consumer advocacy groups: would challenge high barriers and support alternative delivery but are excluded from board governance; have formal comment rights but no decision-making power
 *   - Alternative delivery innovators: blocked by statute from competing via lower-credential models; their exclusion is the enforcement mechanism
 *   - Legislative body: formally sovereign over licensing law but subject to organized incumbent opposition and weak consumer pressure; delegated authority to boards has become capture
 *   - Empirical safety researchers: generate evidence that credential stringency does not correlate with consumer safety; their analysis is available but disempowered against incumbent advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.79).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.81).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.79).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Licensing Mandate: Rent-Seeking Suppression Reading").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/regulatory/labor").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '2a32135b-d3ea-4d8d-ac68-6ac6144928b6').
narrative_ontology:cs_kernel_codification('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', formalized).
narrative_ontology:cs_authority_grounding('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', extraction).
narrative_ontology:cs_interpretation_layer_present('2a32135b-d3ea-4d8d-ac68-6ac6144928b6').
narrative_ontology:cs_reading_relation('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', foundational, credential_restrictions_extract_incumbent_rents).
narrative_ontology:cs_axiom_status(credential_restrictions_extract_incumbent_rents, holdable).
narrative_ontology:cs_axiom_grounding('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', credential_restrictions_extract_incumbent_rents, empirically_contingent).
narrative_ontology:cs_axiom('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', foundational, founding_problem_information_asymmetry_is_dead).
narrative_ontology:cs_axiom_status(founding_problem_information_asymmetry_is_dead, holdable).
narrative_ontology:cs_axiom_grounding('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', founding_problem_information_asymmetry_is_dead, empirically_contingent).
narrative_ontology:cs_reference_frame('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', practitioner_income_protected_by_statutory_supply_restriction).
narrative_ontology:cs_drift_state('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', contemporary_deregulation_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a32135b-d3ea-4d8d-ac68-6ac6144928b6', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, potential_labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_via_price_inflation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established professionals in licensed fields (law, medicine, skilled trades, accounting, etc.) who benefit from statutory credential requirements that restrict labor supply and keep professional income elevated. They set examination standards through professional associations, dominate licensing boards through self-regulation rules, calibrate examination difficulty to maintain target failure rates, and lobby legislatures to expand credential requirements and enforce restrictions against unlicensed alternatives. Their arbitrage capacity is high: they can shift regulatory interpretation, influence legislative agenda, control technical content of examinations, and shape board decision-making. Exiting the constraint is not an option available to them—they are the constraint's primary architects and beneficiaries. The high income they command is dependent on maintained scarcity; losing licensing restrictions would compress their professional income toward competitive levels.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, agenda_setter).

% Individuals seeking to enter a licensed profession face artificially high barriers to entry designed to restrict supply: expensive educational requirements (often longer than necessary for competence), costly examination preparation and fees, calibrated examination failure rates that deny entry even to competent applicants, credential stacking that accumulates requirements without proportional safety value, and continuing education mandates that extend the cost of maintaining licensure indefinitely. Examination passing rates and educational pathway difficulty are set by incumbent practitioners and are maintained at levels that restrict throughput rather than certify minimum competence. Their only path to legitimate practice is conformity to incumbent-designed standards; no alternative pathways exist by law. Exit means abandoning the career entirely—relocation to other jurisdictions with lower barriers is an option but requires geographic mobility that many lack. The constraint traps them because occupational licensing is state-based and geographically uniform; they cannot arbitrage. The cost of entry (time, money, foregone income during education) is artificially inflated by credential requirements.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, potential_labor_market_entrants, payer,
    powerless, biographical, trapped, national).

% End consumers of licensed services (legal services, medical services, skilled trades, accounting, etc.) pay elevated prices because artificial scarcity restricts supply and permits incumbent practitioners to command higher fees. The restriction passes through as a price premium that consumers bear. Most licensed services are non-optional (medical care, legal representation in court, housing construction/inspection, etc.); consumers cannot exit the need for the service. They have no substitute supply because the statute prohibits unlicensed alternatives and blocks lower-credential delivery models. The price inflation is diffuse and unattributed—consumers experience it as the market price of professional services rather than as extraction by the constraint—but the effect is real and persistent. Their constrained exit means they must pay the inflated price or go without the service (which is often not a viable option for legally required services).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_via_price_inflation, payer,
    powerless, immediate, constrained, national).

% Government agencies and professional licensing boards that administer the statutory credentialing requirements. They are nominally neutral adjudicators of standards but are typically composed of sitting practitioners (a self-regulation model) or subject to overwhelming practitioner influence on board decisions. They enforce examination barriers, credential requirements, continuing education mandates, and prosecution of unlicensed practice. They have structural incentive to resist any loosening of standards because incumbent practitioners dominate their governance and provide their political support. Their constrained exit means they cannot easily become pro-consumer or pro-competition without facing organized practitioner opposition that threatens their agency's funding and authority. The formal process for standards revision requires extensive technical testimony that incumbent practitioners are positioned to dominate.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_board_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Organizations that advocate for consumer price reduction, market access expansion, and alternative delivery models would object to restrictive credential requirements but are structurally excluded from meaningful participation in licensing board governance. Their formal right to comment on proposed rules is available but procedurally marginalized; board meetings are technical, testimony is filtered, and composition rules ensure practitioner dominance. Consumer representatives on boards (where they exist) are typically outnumbered and outmatched by practitioner expertise in the technical domain. Their constrained exit means they cannot easily exit the policy space without abandoning consumer interests; they remain engaged despite limited influence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Entrepreneurs and organizations seeking to deliver licensed services through alternative models (partial licensing for specific tasks, lower-credential delivery pathways, task delegation to less-credentialed workers, technology-enabled alternatives) are barred by the statute. The licensing mandate defines what counts as legitimate practice; alternatives that would increase supply and reduce prices are prohibited by law, not merely discouraged by market forces. Their exclusion is not incidental to the constraint—it is the enforcement mechanism itself. They are trapped because the statute leaves no legal pathway for their alternative business model; exit means abandoning the entire service delivery concept.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, alternative_delivery_innovators, excluded,
    moderate, biographical, trapped, national).

% The legislature or regulatory body that enacted the licensing statute and delegates implementation authority to the licensing board. Formally this is where the rules originate and where sovereign power to revise them resides. However, regulatory capture by incumbent practitioners means the effective agenda-setting has migrated to the professional association and the licensing board; legislative revisions are rare and faced with organized practitioner opposition. Legislators face weak consumer pressure (dispersed, unorganized) and strong professional pressure (concentrated, well-funded), creating political incentive to defer to incumbent preference. Their analytical position gives them the power to revise the statute but the political economy constrains their use of it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, legislative_body, agenda_setter,
    institutional, generational, analytical, national).

% Academic researchers, policy analysts, and empirical investigators who study the relationship between licensing stringency (credential requirements, examination standards, etc.) and consumer safety outcomes. They generate evidence on whether high credential barriers correlate with improved safety or with artificial scarcity and price inflation without measurable safety benefit. They conduct comparative analysis of restrictive-licensing versus permissive-licensing jurisdictions, analyze examination question design and failure-rate calibration, and study alternative-delivery models. Their analysis is not binding on licensing boards and they have no formal power to change standards, but they provide evidentiary input to legislative oversight and external critique. Their analytical position means they can observe and measure but not directly alter the constraint.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, empirical_safety_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Statutory licensing coordinates a presumption: that a consumer checking a credential can be confident the bearer meets a minimum standard. In principle, this solves information asymmetry between practitioners and consumers about competence.
% TRANSFER_FUNCTION: Moves income from consumers (via price inflation from restricted supply) and potential entrants (via blocked access and credential costs) to incumbent practitioners. The mechanism is artificial scarcity: by restricting who can legally practice, the statute reduces supply, permits incumbent practitioners to command higher fees, and extracts the difference from consumers and aspiring entrants. The credential requirement functions as a supply cartel enforced by law.
% ABSENT_VOICES: Consumers who would demand lower-cost alternatives, alternative delivery models that could increase supply, and potential practitioners excluded by examination barriers are structurally locked out of licensing board governance. Consumer representatives and evidence from alternative-licensing jurisdictions are formally available for input but procedurally marginalized. The board composition is typically dominated by sitting practitioners (self-regulation model), guaranteeing absent consumer voice.
% DISAPPEARANCE_RATIONALE: If the statutory credential requirement vanished, supply would increase: alternative delivery models would enter the market, examination barriers would disappear, credential stacking would unwind, and professional income would compress as supply expanded. Incumbent practitioner income would fall significantly. Consumer prices would decline as competition increased and artificial scarcity dissolved. The profession would reorganize around competitive supply rather than protected cartel pricing.
% FOUNDING_PROBLEM: When the profession's original licensing statutes were enacted (most US professions licensed in the late 1800s–early 1900s), information asymmetry was severe: consumers had no reliable way to distinguish competent practitioners from fraudsters or charlatans.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners claim the founding problem is still live, citing ongoing fraud risk and consumer vulnerability. Economists, consumer advocates, and alternative-delivery researchers attest the founding problem is substantially solved: reputation systems (reviews, referral networks, malpractice insurance), transparency mechanisms, and verifiable quality signals now communicate competence without legal supply restriction. Jurisdictions with less restrictive licensing (nurse practitioners in expanded scope-of-practice states, mortgage brokers post-2008 deregulation) show that lower barriers and higher supply are compatible with maintained safety outcomes.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.61→0.79 over the interval) because the rent extraction per transaction (the income premium from restricted supply) is large and durable. Suppression is high (0.81) because the constraint's persistence depends entirely on actively excluding alternatives—examination barriers must be maintained, credential stacking must be enforced, unlicensed practice must be prosecuted, and alternative delivery models must be blocked. Theater is elevated and rising (0.42→0.58) because the mounting share of enforcement activity is dedicated to defending supply restriction rather than verifying safety: credential inflation (adding requirements with minimal safety value), examination difficulty calibrated to fail-rate targets (not competence benchmarks), and continuing education mandates designed as toll collection rather than skill maintenance are all performative supply defense. Theater plateaus near 0.58 because the basic enforcement machinery (examination, prosecution of unlicensed practice, statute renewal) must continue regardless; a snare this old and entrenched cannot reach high theater ratios without collapse. The measurement series is shared across all three metrics on one time grid (every metric measured at every point) to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent practitioners' and licensing board administrators' seats, the constraint is genuine coordination—they can point to examination content that certifies minimum competence, enforcement against clear incompetence, and individual cases where licensing prevented harm. From the potential entrants' and consumers' seats, the same structure operates as enforced extraction—the examination barriers are cost-inflating rather than safety-justifying, the enforcement is supply-restricting rather than safety-enforcing, and alternatives that would increase supply while maintaining safety are banned by law. The engine's per-seat computation should show a snare classification for entrants and payer seats (high directionality toward target), a beneficial classification for incumbent practitioners (low directionality, high capture of rents), and a payer classification for consumers (high directionality, diffuse extraction that passes through as price inflation). The architectural asymmetry is the difference between who sets the rules (incumbents via regulatory capture) and who bears the cost (entrants via blocked access, consumers via price inflation).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners are structural beneficiaries with arbitrage capacity: they set examination standards through professional associations, capture the licensing board through self-regulation rules, and can adapt the constraint's intensity to maintain their income advantage. Their directionality is near 0.0 (full beneficiary). Potential entrants are structural targets with trapped exit: the statute is the only path to legitimate practice in their jurisdiction; all alternatives (interstate practice, unlicensed practice, alternative delivery models) are blocked by law. Their directionality is near 1.0 (full target). Consumers are distributed targets with constrained exit: they cannot avoid needing the licensed service (most services are non-optional) and cannot substitute unlicensed alternatives (which are banned). Their directionality is 0.85–0.95 (high target, constrained exit amplifies extraction). The licensing board administrators occupy a captured agenda-setter seat: they are formally neutral but structurally incentivized by incumbent practitioner opposition to any loosening of standards. Their directionality is near 0.2–0.3 (slight target pressure from consumer advocates, strong beneficiary pressure from incumbents).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy misclassification by explicitly identifying the founding problem as dead (information asymmetry substantially solved by modern reputation systems and transparency) while the constraint persists for rent collection. The reading would be falsely classified as rope or coordination if the founding problem were live and the constraint were solving it; mandatrophy resolution requires acknowledging that the original problem is solved but the constraint survives because it now serves a different function (income protection for incumbents). The empirical question is whether examination difficulty, credential stacking, and continuing education mandates still serve the original safety coordination function or have drifted toward pure supply restriction. The evidence (rising theater ratio, compression of extractiveness as examination standards tighten without demonstrable safety improvement, alternative-delivery jurisdictions showing safety maintained at lower barriers) supports the mandatrophy reading: the founding problem has died, but the statutes persist as income-protection mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_death_contestation,
    'Has the founding problem (information asymmetry between practitioners and consumers about competence) been substantially solved by reputation systems, transparency, and quality signaling mechanisms, such that credential stringency is no longer justified by consumer protection, or is the founding problem still live and credential requirements remain necessary?',
    'Comparative analysis of safety outcomes (error rates, complaint ratios, malpractice claims, regulatory action) between jurisdictions with restrictive licensing (high barriers, strict examination) and jurisdictions with permissive licensing (lower barriers, alternative credentials, expanded scope-of-practice). Natural experiments from scope-of-practice regulatory changes. International comparison of credential requirements and consumer safety metrics.',
    'If the founding problem is substantially solved: the constraint reclassifies from coordination/rope to snare or piton (rent extraction with cover story). If the founding problem is still live: the constraint may be tangled_rope or rope depending on whether benefits (consumer safety) justify costs (restricted supply/price inflation). This is the empirical gate on mandatrophy certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_death_contestation, empirical, 'Whether the founding problem of information asymmetry has been solved, making credential requirements no longer safety-justified.').

omega_variable(
    regulatory_capture_extent,
    'To what extent does regulatory capture of licensing boards by incumbent practitioners explain the persistence and inflation of credential requirements, versus alternative explanations such as genuine safety concerns or path dependence?',
    'Analysis of licensing board composition (percentage of positions held by sitting practitioners), decision-making patterns (examination cutoff score setting, credential requirement inflation, alternative-delivery approval rates), and practitioner association lobbying expenditure and legislative influence. Comparison of regulatory drift in states with capture-limiting structures (consumer representatives on boards, public oversight) versus unprotected states.',
    'If capture is extensive: the constraint''s persistence and intensification is explained by incumbent self-interest, supporting the snare/rent-seeking reading. If capture is minimal: other mechanisms (safety concerns, path dependence, professional norms) dominate, supporting alternative readings. The theater ratio and extraction trajectory provide diagnostic signals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Whether incumbent practitioner control of licensing boards explains credential inflation and supply restriction.').

omega_variable(
    examination_calibration_intent,
    'Are licensing examinations designed and calibrated to verify minimum competence for safe practice, or are they calibrated to maintain a target failure rate (supply restriction target) independent of competence measures?',
    'Item-level analysis of examination question design, difficulty trends over time, and failure rate data. Comparison of examination difficulty and failure rates across jurisdictions with different credential inflation trends. Analysis of whether examination board members adjust questions to maintain target failure rates or to match evolving competence standards.',
    'If calibrated to competence: examinations serve the public safety reading (certification of minimum standards). If calibrated to failure-rate targets: examinations serve the snare reading (supply restriction mechanism disguised as quality assurance). Theater ratio should correlate strongly with this finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(examination_calibration_intent, empirical, 'Whether examinations certify competence or enforce supply restrictions via failure-rate calibration.').

omega_variable(
    alternative_delivery_harm_evidence,
    'In jurisdictions where alternative delivery models exist (expanded scope-of-practice, partial licensing, lower-credential alternatives), do consumer harm rates increase, decrease, or remain stable compared to restrictive-licensing jurisdictions?',
    'Comparative safety outcome analysis (adverse events, complaints, regulatory action rates) from states with expanded nurse practitioner scope-of-practice, permissive lending practices, expedited contractor licensing, and other lower-barrier models. Meta-analysis of alternative-delivery trials and natural experiments.',
    'If harm rates increase with lower barriers: the public safety reading is supported (restrictions prevent measurable harm). If harm rates decrease or remain stable: the founding problem is solved and alternative delivery is compatible with consumer protection, supporting the snare reading (safety restriction is not the binding constraint; rent extraction is).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_delivery_harm_evidence, empirical, 'Whether lower credential barriers correlate with increased consumer harm or maintain safety outcomes.').

omega_variable(
    credentialing_drift_justification,
    'When credential requirements inflate over time (additional certifications, continuing education hour increases, examination difficulty increases), are these changes justified by documented safety improvements or by incumbent pressure to maintain scarcity and income?',
    'Time-series analysis of credential requirement changes correlated with safety outcomes (adverse events, harm rates) and incumbent income trends. Analysis of professional association advocacy for credential inflation. Comparison of credential inflation rates across professions with different safety risk profiles (high-risk professions should show slower credential drift if safety-driven; uniform drift across all professions suggests scarcity-driven inflation).',
    'If drift is safety-justified: credential inflation is serving public protection, supporting coordination readings. If drift is scarcity-driven: credential inflation is the primary extraction mechanism, supporting the snare reading. The rising theater ratio (0.42→0.58) suggests mounting performative activity inconsistent with safety justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_drift_justification, empirical, 'Whether credential inflation is justified by safety improvements or driven by incumbent scarcity maintenance.').

omega_variable(
    reading_contest_framing,
    'This constraint is read by incumbents as public_safety_coordination and by critics as rent_seeking_suppression. Is one reading foreclosed by the other, or do both remain coherent within different value commitments (coexists_with)?',
    'Logical analysis of the foundational premises. If one reading''s core claim directly contradicts the other''s such that no single framework could hold both, they foreclose each other. If both readings rest on different value commitments (safety paramount vs. supply elasticity paramount) that do not logically exclude each other, they coexist. Empirical data on the founding problem and regulatory capture settles which reading better predicts outcomes, but does not settle whether both can be held simultaneously.',
    'If foreclosure: the readings are in logical contradiction and cannot both be true (affects the CS axioms classification). If coexistence: the readings are contestable-but-coherent commitments held by different parties, and both names persist in regulatory discourse. This determines the cs_structure.reading_relations classification and affects mandatrophy computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_framing, conceptual, 'Whether the rent-seeking and public-safety readings logically foreclose each other or coexist as contestable framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 5, 0.45).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.49).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 15, 0.52).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.55).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 25, 0.57).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.58).
narrative_ontology:measurement(lice_tr_t35, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 35, 0.58).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.61).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(lice_be_t35, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 35, 0.79).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(lice_su_t35, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 35, 0.81).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, resource_allocation).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.18).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% The licensing statute mandate is a contested kernel with three distinct readings. All three stories share the same statutory requirement (the kernel) but interpret its function and effect differently. The rent_seeking_suppression reading (this story) treats supply restriction and incumbent income protection as the primary function; the public_safety_coordination reading treats consumer protection as primary; the graduated_access_filter reading treats class-stratified access as primary. Each reading has its own constraint_id, its own ε (base extractiveness), its own stakeholder situation descriptions, and its own set of omegas documenting the contestation. The three stories are linked via network.affects_constraints to show their sibling relationship and shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
