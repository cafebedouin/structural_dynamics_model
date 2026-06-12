% ============================================================================
% CONSTRAINT STORY: eldercare_guardianship_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eldercare_guardianship_capture, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eldercare_guardianship_capture
 *   human_readable: Professional Guardianship Capture of Elder Estates
 *   domain: elder_law/fiduciary_capture
 *
 * SUMMARY:
 *   Professional guardianship in the United States has transformed from a
 *   narrow mechanism for protecting genuinely incapacitated elders into a
 *   systemic extraction apparatus capturing estates from vulnerable
 *   individuals and disinheriting families. The constraint exhibits radically
 *   different coercive pressures at different social levels: at the
 *   individual level, accessibility collapse (0.92) and stakes inflation
 *   (0.92) are near-total—a cognitively declining elder loses all legal
 *   capacity to petition for removal, all control over asset liquidation, and
 *   all communication autonomy, compressed into weeks; at the organizational
 *   level (APS agencies, guardianship companies), collapse and inflation are
 *   moderate (0.32-0.42)—these actors navigate the system with significant
 *   agency; at the class level (elder population as a category), capture is
 *   even less visible (0.25)—demographic vulnerability is diffuse and
 *   political mobilization is impeded by the cognitive condition of the
 *   victims themselves; at the structural level, the guardianship system
 *   appears stable and well-designed (0.18)—the judicial review doctrine
 *   looks neutral and protective in the formal apparatus. This gradient
 *   defines the constraint: maximum coercive pressure is concentrated at the
 *   level of the individual ward, minimum pressure is visible at the system
 *   level, and intermediate organizations experience manageable constraints
 *   that align with profit extraction. The grid reveals that the same
 *   institutional design produces radically different lived realities
 *   depending on social position.
 *
 * KEY AGENTS:
 *   - Incapacitated Elder: Individual ward. Powerless/trapped. Loses legal capacity to petition for removal, communication rights are restricted, assets liquidated under guardian control without meaningful consent. Accessibility collapse 0.92, stakes inflation 0.92 at individual level.
 *   - Disinherited Family Member: Adult child or heir. Moderate/constrained. Can attempt guardianship removal through probate litigation but faces $10k-50k+ legal costs, 2-4 year timelines, and judicial deference doctrine that presumes the guardian acted properly. Successfully contest only ~5-10% of cases.
 *   - Adult Protective Services Agency: Organized actor. Mixed coordinator and passive overseer. Identifies and reports incapacity but lacks post-appointment monitoring capacity. Judicial deference means APS involvement ends once guardianship is appointed.
 *   - Professional Guardianship Enterprise: Institutional beneficiary. For-profit and non-profit companies, individual professional guardians, attorney networks. Arbitrage exit (can leave market easily). Benefit from fee structures (5-10% of estate annually), minimal oversight, and judicial deference. Accessibility collapse is low (0.32) at organizational level.
 *   - Probate Court System: Institutional gatekeeper. Appoints guardians and theoretically oversees them, but lacks verification capacity. Theater ratio is high (0.65)—judicial review proceedings occur (forms filed, accountings received) but judges rely on presumption-of-correctness and lack investigative resources. Oversight function is atrophied.
 *   - Analytical Observer: Civilizational perspective. Risks naturalizing a contingent institutional arrangement (fee-for-service guardianship, burden-shifting doctrine, lack of monitoring) as an inherent feature of elder protection law. False summit candidate.
 *   - State Legislature: Structural level. Sets guardianship statute, fee caps, monitoring requirements, burden-of-proof rules, conflict-of-interest limitations. Pressure at structural level is low (0.22 suppression, 0.38 resistance) because the system appears stable and legitimate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eldercare_guardianship_capture, 0.78).
domain_priors:suppression_score(eldercare_guardianship_capture, 0.82).
domain_priors:theater_ratio(eldercare_guardianship_capture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eldercare_guardianship_capture, extractiveness, 0.78).
narrative_ontology:constraint_metric(eldercare_guardianship_capture, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(eldercare_guardianship_capture, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eldercare_guardianship_capture, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(eldercare_guardianship_capture, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eldercare_guardianship_capture, snare).
narrative_ontology:human_readable(eldercare_guardianship_capture, "Professional Guardianship Capture of Elder Estates").
narrative_ontology:topic_domain(eldercare_guardianship_capture, "elder_law/fiduciary_capture").

domain_priors:requires_active_enforcement(eldercare_guardianship_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eldercare_guardianship_capture, professional_guardians).
narrative_ontology:constraint_beneficiary(eldercare_guardianship_capture, attorney_networks).
narrative_ontology:constraint_victim(eldercare_guardianship_capture, incapacitated_elders).
narrative_ontology:constraint_victim(eldercare_guardianship_capture, disinherited_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eldercare_guardianship_capture, professional_guardian).
narrative_ontology:constraint_beneficiary(eldercare_guardianship_capture, attorney_network).
narrative_ontology:constraint_victim(eldercare_guardianship_capture, incapacitated_elder).
narrative_ontology:constraint_victim(eldercare_guardianship_capture, disinherited_family).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An adult (typically age 75+) with diagnosed cognitive decline (dementia, Alzheimer's, severe mental illness). Loses all legal capacity to manage finances or healthcare decisions. Assets—home, savings, investments—are placed under guardian control. Communication with family is restricted via guardian-imposed contact limitations. No capacity to petition for guardianship removal; any petition would need to come from family and must overcome burden-of-proof doctrine. Experiences estate liquidation, asset sales at disadvantageous prices, and daily life decisions made unilaterally by the guardian. Trapped in every sense: legal incapacity, no communication alternatives, no economic resources to contest the arrangement.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, incapacitated_elder, payer,
    powerless, biographical, trapped, local).

% Adult child or heir who seeks to contest guardianship or recover assets. Has legal standing to petition for removal but faces severe constraints: probate litigation costs $10k-50k+, timelines extend 2-4 years, judicial deference doctrine creates near-impossible evidentiary burden (must prove clear and convincing evidence of guardian misconduct), and court-appointed evaluation of the ward's capacity is usually controlled by the same guardian's preferred evaluators. Family members are typically excluded from financial information—accountings are filed with the court but families must file additional motions to view them. Even successful removal typically occurs only after 50-80% of the estate has been depleted through guardian fees and asset liquidation. The exit is expensive and often fails; families frequently abandon contestation attempts after initial consultation with attorneys who quote prohibitive costs.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, disinherited_family, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eldercare_guardianship_capture, disinherited_family, excluded).

% State Adult Protective Services agency tasked with identifying and protecting vulnerable elders from abuse and neglect. Investigates elder abuse reports and, when a person is deemed incapacitated with no responsible family member available, petitions for guardianship appointment. APS role terminates once guardianship is appointed; post-appointment oversight is the court's responsibility. APS agencies are typically understaffed (average caseload per investigator: 40-80 active cases), lack resources for follow-up investigations, and depend on guardians to provide information about the ward's condition and finances. Some APS agencies have developed relationships with specific professional guardians, creating implicit coordination where APS refers cases to guardians who manage them according to APS expectations. Communication between APS and families is often limited; families are rarely informed about the guardianship appointment process once initiated.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, aps_agency, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eldercare_guardianship_capture, aps_agency, observer).

% Individual professional guardian or for-profit guardianship company licensed by the state to manage affairs of incapacitated persons. Receives court appointment to serve as guardian for multiple wards simultaneously (ranges from 30-300+ wards per guardian, depending on company size and state regulations). Collects guardian fees (typically 5-10% of annual estate value, ranging $2,000-$50,000+ per year depending on estate size), attorney fees for routine matters, and commissions on asset sales. Can exit the market easily by declining new appointments or allowing license to lapse; no exit barriers. Operates with minimal oversight—court review is rare unless a removal petition is filed. Benefits from information asymmetry: families lack access to financial information; court review assumes guardian correctness. Can restrict ward contact with family members and influence evaluators who assess ward capacity. Extracts maximum rents within the bounds of what would trigger judicial notice.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, professional_guardian, beneficiary,
    institutional, immediate, arbitrage, regional).

% Attorneys who petition for guardianship appointments and represent guardians in probate court proceedings. Collect fees for guardianship petitions ($1,500-$5,000 per petition), representation at guardianship hearings, annual accountings preparation, and other routine matters. Often work in repeating relationships with specific professional guardians and APS agencies, creating networks of mutual referral and support. Have no obligation to represent the interests of the proposed ward—court appointment is the sole interest. Exit cost is low; can decline guardianship representation easily. Benefit from growth in guardianship appointments and from defending guardianships against removal petitions (which generate billable hours). Some attorneys simultaneously serve as guardians, creating conflicts of interest (attorney collects attorney fees AND guardian fees for the same ward).
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, attorney_network, beneficiary,
    institutional, immediate, arbitrage, regional).

% State probate court judges and court administration who appoint guardians, receive annual accountings and hear removal petitions. Responsible for oversight of guardianship arrangements and protection of ward interests. In practice, probate courts operate under significant resource constraints: judges handle large caseloads (hundreds of probate matters per judge per year), court staff are insufficient to audit accountings or investigate guardian behavior, and removal petitions are rare (occur in <5% of guardianships). Judicial doctrine places presumption-of-correctness on guardian actions, meaning a person challenging the guardianship must prove clear and convincing evidence of misconduct—a standard nearly impossible to meet without expert testimony, which costs money. Courts benefit from guardianship appointments because they reduce court caseload (wards no longer appear in disability or civil commitment proceedings) and insulate courts from political pressure around elder welfare decisions (courts can defer to 'neutral' guardians). Judicial review is therefore largely theatrical: accountings are filed and reviewed for procedural correctness, not audited for accuracy or propriety.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, probate_court, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% State legislative bodies that enact guardianship statutes. Set the rules governing guardianship appointment, fee structures, monitoring requirements, conflict-of-interest limitations, and removal procedures. Many states have weaker monitoring requirements (no mandatory audits, no mandatory court review of accountings, no conflict-of-interest restrictions on attorney-guardians). Legislatures receive relatively little political pressure to reform guardianship law because the affected population (incapacitated elders) cannot mobilize politically and families are dispersed. Professional guardians and attorney networks actively lobby against restrictive legislation. Some legislatures have recently moved to strengthen protections (e.g., Texas requires public guardianship funding, Florida requires monitoring audits), but most states have minimal regulatory architecture. Pressure at the structural level to strengthen oversight is low because the system appears legitimate and the beneficiaries (professional guardians, attorneys) have concentrated interests and institutional access.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, state_legislature, agenda_setter,
    powerful, generational, mobile, national).

% Non-profit organizations and ad-hoc groups advocating for elder rights and against guardianship abuse (e.g., National Center on Law and Elder Rights, state bar elder law sections). Have identified and documented guardianship abuse patterns, published research on extraction mechanisms, and advocated for legislative reform. Excluded from guardianship decision-making: have no standing to participate in guardianship appointments, no automatic notice of hearings, limited resources to intervene in individual cases. Attempt to reform guardianship statutes and create alternatives like supported decision-making and limited guardianship. Meet significant resistance from professional guardians and bar associations who defend the status quo. Advocacy effectiveness is limited by inability to mobilize the primary affected population (incapacitated elders themselves cannot advocate) and by relatively low media and political salience of guardianship issues.
narrative_ontology:constraint_stakeholder(eldercare_guardianship_capture, elder_advocacy_organizations, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eldercare_guardianship_capture, professional_guardian).
narrative_ontology:fixing_cost_class(eldercare_guardianship_capture, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managing financial and personal decisions for cognitively incapacitated adults who have no family member willing or able to assume fiduciary responsibility. Genuine coordination problem: someone must pay bills, manage medical decisions, and authorize healthcare for people who cannot do so themselves.
% TRANSFER_FUNCTION: Money (guardian fees, attorney fees, commissions on asset sales) flows from incapacitated ward's estate to professional guardian, attorney networks, and court system. Labor (day-to-day decision-making about healthcare, residence, asset management) is provided by guardian, with benefits accruing to guardian in the form of fee income and control over asset allocation. Family inheritance is transferred from heirs to guardians (through estate depletion and asset liquidation).
% ABSENT_VOICES: The incapacitated elder cannot participate in the guardianship appointment process (they lack legal capacity to consent or object) and cannot later petition for removal (legal doctrine bars them). Families are excluded from financial information and often excluded from ward contact. Advocates for elder rights are excluded from appointment decisions and removal proceedings. The primary affected population is structurally unable to be present in the conversation about their own guardianship.
% DISAPPEARANCE_RATIONALE: Professional guardians argue that if the guardianship system disappeared, many incapacitated elders without family support would be harmed—bills unpaid, medical decisions unmade, property neglected. Families and advocates counter that supported decision-making and limited guardianship models (with real monitoring) could replace the current system with fewer extraction risks. The dispute centers on whether the current system is the only viable option (beneficiary argument) or whether it is an unnecessarily extractive design (advocate argument). Structural evidence suggests the latter: jurisdictions with stronger monitoring (public guardianship plus mandatory audits) show lower abuse rates, suggesting the extraction is contingent, not inevitable.
% FOUNDING_PROBLEM: Legal guardianship was established to protect incapacitated persons from harm when family members were absent or unwilling to provide care. Original purpose: ensure that vulnerable adults received necessary medical care, bill payment, and property management despite cognitive incapacity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (absence of responsible family) persists for some elders, but professional guardianship has become the default solution even when families ARE present and capable. Most modern guardianship appointments occur when: (1) families exist but dispute over the guardianship appointment itself (guardian is imposed against family objection), (2) families are economically vulnerable and cannot afford to contest, (3) APS agencies prefer guardianship over family arrangements as a risk-management tool (guardianship liability falls on the guardian, not APS). Empirical data from several states shows that family-contested guardianships are often upheld despite family capability and presence. The original founding problem (no one to care for the incapacitated elder) no longer explains most modern appointments; instead, the system is used for risk transfer (liability moves from APS to guardian) and professional rent extraction.
narrative_ontology:disappearance_verdict(eldercare_guardianship_capture, contested).
narrative_ontology:founding_problem_status(eldercare_guardianship_capture, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual with diagnosed cognitive decline or incapacity. Trapped: no legal capacity to petition for removal, no resources to hire counsel, no cognitive ability to initiate contact with outside advocates. Experiences maximum extraction — estate liquidated under guardianship, healthcare decisions made unilaterally, communication restricted. Full snare: alternatives completely collapsed, suppression applied through legal instruments and institutional authority.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Adult child or family seeking to contest guardianship or recover assets. Constrained exit: legal contestation is possible but prohibitively expensive (probate litigation costs $10k-50k+), time-consuming (2-4 years), and often fails due to judicial deference to guardians. Experiences substantial extraction — inheritance liquidated, family relationships disrupted by guardian-imposed contact restrictions. Snare classification: suppression through legal procedure and burden-shifting.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% State agency tasked with elder protection. Mixed coordination and extraction: genuine coordination function (identify and protect genuinely incapacitated elders) exists alongside extraction (APS investigators lack capacity to monitor guardians post-appointment; oversight is theatrical; agency benefits from deferential relationship with appointed guardians who handle caseload). Constrained exit: funding limitations prevent rigorous monitoring; staff turnover undermines continuity; mandatory reporter liability creates incentive to appoint rather than supervise.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% For-profit and non-profit guardianship companies, individual professional guardians, and attorney networks. Rope from their perspective: genuine coordination function (managing estates of incapacitated elders who have no family) exists alongside substantial rent extraction. Arbitrage exit: can exit the market easily (license is optional, alternative practice areas available); experience the constraint as flexible coordination, not coercive extraction. Net beneficiaries: collect fees (typically 5-10% of estate annually), control asset allocation, and operate with minimal judicial oversight.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Judicial system that appoints guardians and theoretically oversees them. Piton: the oversight function is substantially performative. Courts receive and file guardianship accountings but lack capacity to audit them; judges hear removal petitions but rely on presumption-of-correctness doctrine; court clerks process paperwork but lack investigative capacity. Theater ratio high: the judicial review proceeding occurs (theatrical compliance) but lacks real verification. Primary function (protecting vulnerable elders) has atrophied; constraint persists through institutional inertia and judicial deference.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational view, some legal guardianship is inherent to protecting incapacitated persons who cannot advocate for themselves — this is presented as a natural law of elder protection. However, the constraint structure reveals this as a false summit: the extraction mechanism, suppression of family contestation, theatrical oversight, and financial abuse patterns are contingent on specific institutional design choices (fee structures, burden-shifting doctrine, lack of monitoring infrastructure), not on the logical necessity of guardianship itself.
constraint_indexing:constraint_classification(eldercare_guardianship_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eldercare_guardianship_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eldercare_guardianship_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eldercare_guardianship_capture, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eldercare_guardianship_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eldercare_guardianship_capture, TR),
    TR >= 0.70.

:- end_tests(eldercare_guardianship_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and rising. Guardianship capture exhibits systematic estate depletion through: (1) guardian fees (5-10% of assets annually, totaling 50-100% over 10 years for estates under management from age 82-92); (2) attorney fees charged against estates for routine matters; (3) asset sales below market value to companies affiliated with the guardian; (4) liquidation of real property at fire-sale prices under time pressure. The rising trajectory (0.55→0.78 over the interval) reflects increasing sophistication of extraction mechanisms and accumulation of case-specific institutional knowledge by professional guardians. Suppression (0.82): Very high. Multiple suppression mechanisms operate simultaneously: (1) legal incapacity doctrine bars the ward from petitioning for removal; (2) burden-shifting—removal petitions are presumed invalid without clear and convincing evidence the guardian acted improperly (near-impossible standard); (3) communication restriction—guardians can limit ward contact with family members; (4) information asymmetry—families are excluded from financial statements and court filings; (5) cost barrier—contestation costs exceed most families' resources. Theater ratio (0.65): Moderate-high. Judicial oversight is substantially performative—accountings are filed but not audited, court review occurs but relies on presumptions of guardian correctness, removal petitions are heard but face judicial deference doctrine. Professional guardians present themselves as neutral fiduciaries but operate as private contractors extracting maximum rents. APS presentations frame guardianship as 'protection' while lacking capacity to supervise. The performative layer masks the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the individual ward's experience (snare: total accessibility collapse, zero agency, maximum suppression) and the structural system's appearance (mountain/rope: stable institution, natural law of elder protection, effective guardianship system). The ward cannot perceive the constraint as anything other than a maximum extraction mechanism—the suppression is comprehensive. The family member perceives tangled_rope (some coordination, significant extraction). The professional guardian perceives rope (coordination function they provide, benefits they collect). The court perceives piton (review procedures that look protective but are substantially theatrical). The analytical observer at the civilizational level risks the deepest error: naturalizing the institutional arrangement as an inherent feature of elder protection law, when in fact the extraction architecture is contingent on specific design choices (fee-for-service, burden-shifting doctrine, lack of monitoring). This gap—between maximum coercive pressure on the individual and diffuse appearance at the structural level—is the diagnostic signature of snare operating through institutional architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically by social level. Individual level: the incapacitated elder is a full target (d ≈ 1.0)—maximum extraction, complete exit closure (trapped status), full suppression. Organizational level: APS agencies and guardianship companies experience moderate d (0.4-0.6)—they benefit from the constraint while also bearing coordination costs. The professional guardian is the primary beneficiary (d ≈ 0.0), arbitrage exit, full rent collection. Structural level (judiciary): moderate d (0.5-0.6)—courts coordinate elder protection function but also benefit from guardianship appointments (caseload reduction, political insulation from elder welfare decisions). The grid reveals that effective extraction is concentrated at the individual level (high d, high suppression, high stakes inflation), while at organizational and structural levels, agents navigate the constraint with significant agency (moderate to low d, lower suppression, lower stakes). This differentiation is the core insight: the same institutional structure produces maximal extraction at the individual level because that is where d is pushed to 1.0 through legal incapacity doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is partially resolved but with pathology. The original mandate—'protect incapacitated elders from self-harm and support decision-making for those unable to decide'—is genuine and live. However, the mandate has spawned an extraction apparatus: professional guardians, attorney networks, and court systems have developed institutional interests in maintaining guardianship appointments and fee-collection. Judicial deference doctrine emerged (burden-shifting, presumption-of-correctness) to protect guardians from contestation, which was rationally designed to prevent frivolous removal petitions but empirically suppresses legitimate ones. The result: the mandate persists as cover, but the primary function has become rent extraction. The constraint cannot be classified as a single type because the mandate has fractured into two structural realities: (1) genuine coordination function (elders need fiduciary support), which appears as rope or tangled_rope depending on perspective; (2) extraction apparatus (guardian networks extract rents from estates), which appears as snare or piton depending on perspective. The mandatrophy is NOT resolved—it is masked by the performative layer (judicial review, fiduciary duty, professional standards) while the extraction machinery operates underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incapacity_determination_accuracy,
    'How many guardianship petitions involve genuine incapacity vs. cases where cognitive decline is minimal or contested by family?',
    'Retrospective review of guardianship petitions and subsequent neurocognitive evaluations; comparison of initial assessment severity with post-guardianship functional ability claims; family contestation rates and outcomes',
    'If genuine incapacity in >85% of cases: extraction is narrow (only compensating for real inability). If genuine incapacity in <65%: many guardianships are vehicles for non-consensual asset control; snare classification strengthens. If middle ground: suggests gatekeeping failure and intake bias toward conservatorship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incapacity_determination_accuracy, empirical, 'What proportion of guardianship cases involve genuine incapacity').

omega_variable(
    natural_law_vs_institutional_design,
    'Is the extraction mechanism inherent to guardianship law itself, or contingent on specific design choices (fee structures, burden-shifting, lack of monitoring)?',
    'Comparative institutional analysis: jurisdictions with strong monitoring (e.g., public guardianship + mandatory audits) vs. jurisdictions with weak monitoring; correlation between fee structures and extraction severity; relationship between guardianship removal difficulty and measured abuse rates',
    'If inherent (unavoidable): mountain classification holds. If contingent: false summit detected; redesign pathways exist (e.g., supported decision-making, limited guardianship with monitoring, fee caps). This omega directly addresses whether the constraint is a natural law or a capture mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_design, conceptual, 'Whether guardianship capture is inherent to the institution or contingent on design').

omega_variable(
    family_contestation_bias,
    'Are family members less likely to contest guardianship when they are uninformed, geographically distant, or economically vulnerable—creating a selection effect where only wealthy families mount successful challenges?',
    'Analysis of successful vs. unsuccessful guardianship removal petitions; correlation with family income, education, proximity to court; qualitative interviews with families who abandoned contestation attempts; comparison of family composition in successful vs. failed challenges',
    'If strong selection bias: snare classification strengthens (system suppresses lower-class contestation). If weak bias: suggests gate failure is broader (even informed families cannot overcome burden-shifting). Either outcome supports snare; the mechanism differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_contestation_bias, empirical, 'Whether family contestation success correlates with socioeconomic status').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Are elders suppressed from leaving guardianship through structural barriers (legal incapacity doctrine) or through cognitive barriers (isolation, medication, communication restriction)?',
    'Case analysis of release requests by wards; documentation of communication patterns before/after guardianship appointment; correlation between contact restriction orders and ward incapacity claims; interviews with released wards about their cognitive state during guardianship vs. after exit',
    'If predominantly structural: the legal system itself is the suppression mechanism; reform pathway involves burden-shifting doctrine. If predominantly cognitive: the guardian''s actions (isolation, medication management) are the suppression mechanism; remedy involves monitoring and removal authority. Mixed mechanisms require layered intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether suppression is structural or internalized/practiced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eldercare_guardianship_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(egc_tr_t0, eldercare_guardianship_capture, theater_ratio, 0, 0.5).
narrative_ontology:measurement_basis(egc_tr_t0, observed).
narrative_ontology:measurement(egc_tr_t3, eldercare_guardianship_capture, theater_ratio, 3, 0.58).
narrative_ontology:measurement_basis(egc_tr_t3, observed).
narrative_ontology:measurement(egc_tr_t6, eldercare_guardianship_capture, theater_ratio, 6, 0.65).
narrative_ontology:measurement_basis(egc_tr_t6, observed).
narrative_ontology:measurement(egc_tr_t10, eldercare_guardianship_capture, theater_ratio, 10, 0.65).
narrative_ontology:measurement_basis(egc_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(egc_be_t0, eldercare_guardianship_capture, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(egc_be_t0, observed).
narrative_ontology:measurement(egc_be_t3, eldercare_guardianship_capture, base_extractiveness, 3, 0.68).
narrative_ontology:measurement_basis(egc_be_t3, observed).
narrative_ontology:measurement(egc_be_t6, eldercare_guardianship_capture, base_extractiveness, 6, 0.78).
narrative_ontology:measurement_basis(egc_be_t6, observed).
narrative_ontology:measurement(egc_be_t10, eldercare_guardianship_capture, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(egc_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(egc_su_t0, eldercare_guardianship_capture, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(egc_su_t0, observed).
narrative_ontology:measurement(egc_su_t3, eldercare_guardianship_capture, suppression_requirement, 3, 0.78).
narrative_ontology:measurement_basis(egc_su_t3, observed).
narrative_ontology:measurement(egc_su_t6, eldercare_guardianship_capture, suppression_requirement, 6, 0.82).
narrative_ontology:measurement_basis(egc_su_t6, observed).
narrative_ontology:measurement(egc_su_t10, eldercare_guardianship_capture, suppression_requirement, 10, 0.82).
narrative_ontology:measurement_basis(egc_su_t10, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=10
narrative_ontology:measurement(egc_grid_01, eldercare_guardianship_capture, accessibility_collapse(class), 0, 0.22).
narrative_ontology:measurement(egc_grid_02, eldercare_guardianship_capture, accessibility_collapse(class), 10, 0.25).
narrative_ontology:measurement(egc_grid_03, eldercare_guardianship_capture, accessibility_collapse(individual), 0, 0.85).
narrative_ontology:measurement(egc_grid_04, eldercare_guardianship_capture, accessibility_collapse(individual), 10, 0.92).
narrative_ontology:measurement(egc_grid_05, eldercare_guardianship_capture, accessibility_collapse(organizational), 0, 0.28).
narrative_ontology:measurement(egc_grid_06, eldercare_guardianship_capture, accessibility_collapse(organizational), 10, 0.32).
narrative_ontology:measurement(egc_grid_07, eldercare_guardianship_capture, accessibility_collapse(structural), 0, 0.15).
narrative_ontology:measurement(egc_grid_08, eldercare_guardianship_capture, accessibility_collapse(structural), 10, 0.18).
narrative_ontology:measurement(egc_grid_09, eldercare_guardianship_capture, resistance(class), 0, 0.22).
narrative_ontology:measurement(egc_grid_10, eldercare_guardianship_capture, resistance(class), 10, 0.25).
narrative_ontology:measurement(egc_grid_11, eldercare_guardianship_capture, resistance(individual), 0, 0.08).
narrative_ontology:measurement(egc_grid_12, eldercare_guardianship_capture, resistance(individual), 10, 0.05).
narrative_ontology:measurement(egc_grid_13, eldercare_guardianship_capture, resistance(organizational), 0, 0.25).
narrative_ontology:measurement(egc_grid_14, eldercare_guardianship_capture, resistance(organizational), 10, 0.28).
narrative_ontology:measurement(egc_grid_15, eldercare_guardianship_capture, resistance(structural), 0, 0.35).
narrative_ontology:measurement(egc_grid_16, eldercare_guardianship_capture, resistance(structural), 10, 0.38).
narrative_ontology:measurement(egc_grid_17, eldercare_guardianship_capture, stakes_inflation(class), 0, 0.32).
narrative_ontology:measurement(egc_grid_18, eldercare_guardianship_capture, stakes_inflation(class), 10, 0.38).
narrative_ontology:measurement(egc_grid_19, eldercare_guardianship_capture, stakes_inflation(individual), 0, 0.88).
narrative_ontology:measurement(egc_grid_20, eldercare_guardianship_capture, stakes_inflation(individual), 10, 0.92).
narrative_ontology:measurement(egc_grid_21, eldercare_guardianship_capture, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(egc_grid_22, eldercare_guardianship_capture, stakes_inflation(organizational), 10, 0.42).
narrative_ontology:measurement(egc_grid_23, eldercare_guardianship_capture, stakes_inflation(structural), 0, 0.12).
narrative_ontology:measurement(egc_grid_24, eldercare_guardianship_capture, stakes_inflation(structural), 10, 0.15).
narrative_ontology:measurement(egc_grid_25, eldercare_guardianship_capture, suppression(class), 0, 0.38).
narrative_ontology:measurement(egc_grid_26, eldercare_guardianship_capture, suppression(class), 10, 0.45).
narrative_ontology:measurement(egc_grid_27, eldercare_guardianship_capture, suppression(individual), 0, 0.78).
narrative_ontology:measurement(egc_grid_28, eldercare_guardianship_capture, suppression(individual), 10, 0.85).
narrative_ontology:measurement(egc_grid_29, eldercare_guardianship_capture, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(egc_grid_30, eldercare_guardianship_capture, suppression(organizational), 10, 0.48).
narrative_ontology:measurement(egc_grid_31, eldercare_guardianship_capture, suppression(structural), 0, 0.18).
narrative_ontology:measurement(egc_grid_32, eldercare_guardianship_capture, suppression(structural), 10, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eldercare_guardianship_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(eldercare_guardianship_capture, probate_fee_extraction).
narrative_ontology:affects_constraint(eldercare_guardianship_capture, incapacity_determination_gatekeeping).
narrative_ontology:affects_constraint(eldercare_guardianship_capture, judicial_deference_doctrine).

% DUAL FORMULATION NOTE:
% Guardianship capture decomposes into multiple structurally distinct constraints: (1) incapacity determination gatekeeping—who decides cognitive decline is sufficient to trigger guardianship; (2) guardianship appointment and oversight—judicial appointment and review process; (3) fee extraction—the fee structures and absence of monitoring that enable rent collection; (4) removal barriers—burden-shifting doctrine that suppresses legitimate contestation. Each has its own ε value and beneficiary/victim structure. This story focuses on the individual-level coercive pressure differential; linked constraints address upstream gatekeeping and downstream fee extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
