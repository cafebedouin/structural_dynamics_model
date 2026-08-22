% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Regulation of Legal Drug Markets with Quality/Access Controls
 *   domain: public_health/economic_policy/criminal_justice
 *
 * SUMMARY:
 *   This constraint instantiates the LEGALIZATION READING of the contested
 *   substance_control_authority kernel. The state claims authority to
 *   regulate drug markets as legal commerce with quality/access controls—a
 *   shift from criminalization (prohibition_reading) and from minimalist
 *   public-health harm reduction (harm_reduction_reading). Users exit both
 *   the criminal prosecution system and the black-market victim set;
 *   regulated market participants and public health authorities become
 *   co-beneficiaries; enforcement burden shifts from criminal investigation
 *   to compliance inspection. The reading's core normative premise is market
 *   regulation superior to prohibition as a mechanism for protecting third
 *   parties while respecting user autonomy. This constraint is NOT neutral
 *   about the founding problem—it frames legalization as the superior
 *   response to prohibition's documented harms. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as tangled_rope (coordination
 *   function + asymmetric extraction), authored as structurally true from the
 *   legalization seat, while the metrics describe moderate extractiveness and
 *   moderate suppression. The prohibition and harm-reduction readings, if
 *   authored, would show different structural relationships and would carry
 *   different metrics. This story describes only this one reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.38).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.22).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Regulation of Legal Drug Markets with Quality/Access Controls").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/economic_policy/criminal_justice").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'b4854c1e-a715-4b21-8a17-739b37376404').
narrative_ontology:cs_kernel_codification('b4854c1e-a715-4b21-8a17-739b37376404', formalized).
narrative_ontology:cs_authority_grounding('b4854c1e-a715-4b21-8a17-739b37376404', extraction).
narrative_ontology:cs_interpretation_layer_present('b4854c1e-a715-4b21-8a17-739b37376404').
narrative_ontology:cs_reading_relation('b4854c1e-a715-4b21-8a17-739b37376404', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4854c1e-a715-4b21-8a17-739b37376404', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('b4854c1e-a715-4b21-8a17-739b37376404', foundational, market_regulation_superior_to_criminalization).
narrative_ontology:cs_axiom_status(market_regulation_superior_to_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('b4854c1e-a715-4b21-8a17-739b37376404', market_regulation_superior_to_criminalization, empirically_contingent).
narrative_ontology:cs_axiom('b4854c1e-a715-4b21-8a17-739b37376404', foundational, state_capacity_for_substance_market_governance).
narrative_ontology:cs_axiom_status(state_capacity_for_substance_market_governance, holdable).
narrative_ontology:cs_axiom_grounding('b4854c1e-a715-4b21-8a17-739b37376404', state_capacity_for_substance_market_governance, instrumental).
narrative_ontology:cs_reference_frame('b4854c1e-a715-4b21-8a17-739b37376404', regulated_legal_market_framework).
narrative_ontology:cs_drift_state('b4854c1e-a715-4b21-8a17-739b37376404', contemporary_legalization_maturity, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('b4854c1e-a715-4b21-8a17-739b37376404', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_market_participants).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_parties_from_illegal_markets).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, black_market_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, criminalized_users_transition_cohort).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, users_exiting_criminal_system).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_parties_protected_from_illegal_markets).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, users_exiting_criminal_system).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, market_regulation_superior_to_prohibition).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, state_capacity_to_manage_substance_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts legal framework permitting controlled drug commerce, issues licenses to suppliers, sets quality/purity standards, manages access restrictions (age, purchase limits, prescription requirements where applicable), collects licensing revenue and tax. Administers compliance inspections and enforcement against unlicensed suppliers. Bears political cost of regulating markets that prohibition framed as intrinsically illegitimate.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Licensed growers, manufacturers, distributors, retailers operate within legal framework. Access to capital, banking, liability insurance, trademark protection, and distribution infrastructure previously unavailable in illegal markets. Compete on product quality and pricing rather than violence. Face compliance costs (testing, labeling, tracking) and regulatory uncertainty about future rule changes. Benefit from elimination of criminal prosecution risk and from stable, predictable market conditions relative to black market competitors.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_market_participants, beneficiary,
    organized, generational, mobile, national).

% No longer face arrest, incarceration, or criminal record for possession/use. Access products of known composition (reduced overdose risk from adulterants). Purchase at regulated venues with consistent potency and safety profiles. May face taxation and access restrictions (licensing-based distribution, age gates, quantity limits). Pay regulated prices rather than black-market markups, but higher than unregulated street prices. Retain use-pattern choice; do not face mandatory treatment unless statutory frameworks require it. Transition cohort carries lingering criminal records despite legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, users_exiting_criminal_system, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, users_exiting_criminal_system, payer).

% Gain regulatory visibility into supply chains, potency profiles, and use patterns (via sales tracking and tax data). Can implement interventions at the supply level: mandated testing, tamper-evident packaging, potency labeling, restrictions on marketing claims. Coordinate with health services on overdose response (access to product composition data enables better emergency protocols). Inherit the political responsibility for managing use-related public health outcomes—failures are now attributed to regulatory design rather than user choice.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_authorities, beneficiary,
    institutional, generational, analytical, national).

% Eliminated or substantially reduced exposure to drug-related crime (trafficking violence, property crime by users funding habit through illegal channels, neighborhood destabilization). Community institutions (schools, workplaces, neighborhoods) experience reduced disruption from criminal drug distribution and associated gang activity. Do not participate in market directly but benefit from externality reduction. Bear residual harms if legalization increases use volume or if regulatory framework fails to control supply distribution.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, third_parties_protected_from_illegal_markets, beneficiary,
    powerless, biographical, trapped, local).

% Lose market access as legal suppliers capture distribution. High-margin illegal operations eliminated by price competition and convenience of legal purchase. May attempt to capture market share in age-restricted or quantity-restricted products; face criminal prosecution for unlicensed supply. Economic incentive is complete elimination (cannot compete on price or legitimacy against regulated suppliers). Some transition into licensed market; others remain in black market for unregulated variants or customer bases excluded by legal-market access restrictions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, black_market_suppliers, payer,
    organized, biographical, constrained, national).

% Carry criminal records incurred under prohibition regime even after legalization; collateral consequences (employment barriers, housing discrimination, voting restrictions, professional licensing bans) persist. Legal pathway forward does not erase past enforcement. Some remain unable to participate in legal market due to identity/background—employers may refuse to hire, landlords to rent—that locks them into continued illegal-market participation or economic marginalization despite legalization. Psychological reorientation required: identity formed under criminalization may be incompatible with legal-market participant status.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, criminalized_users_transition_cohort, payer,
    powerless, biographical, identity_locked, national).

% Organizations (DEA, local police drug units) built on enforcement against illegal drug supply lose primary jurisdiction and resource justification. Would advocate maintaining prohibition or strict enforcement boundaries; legalization removes their core function. Shifts enforcement burden to regulatory compliance inspection rather than criminal investigation—different institutional structure, different career paths, reduced headcount justification.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, enforcement_agencies, excluded,
    institutional, generational, analytical, national).

% Segments that perceive drug use as intrinsically immoral or a sign of personal failure may reject legalization premise. Excluded from the regulatory consensus that constitutes legalization authority; their objections are suppressed via democratic process or institutional override. Fear of increased use volume or perception of government endorsement drives resistance. Would advocate continued prohibition but lack institutional power to block legalization in jurisdictions that move forward.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_opinion_constituencies, excluded,
    powerless, biographical, constrained, national).

% Shift from criminalization-focused or abstinence-mandatory approaches to integration with legal market. Gain resources for evidence-based interventions (medication-assisted treatment, supervised consumption, harm reduction) when drug use is no longer criminalized. Regulatory framework determines whether treatment is mandatory, incentivized, or voluntary; this constraint does not determine those details but creates the institutional space for integration.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, treatment_and_harm_reduction_providers, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, regulated_market_participants).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unregulated criminal supply with state-managed legal commerce: achieves product quality verification, potency labeling, safe distribution, tax revenue generation, and public-health visibility into supply chains. Eliminates violence and adulteration risks endemic to black markets. Coordinates multiple third-party protections (community safety, overdose response capability, treatment integration) through unified regulatory framework rather than fragmented enforcement.
% TRANSFER_FUNCTION: Moves drug supply from criminal networks to licensed operators; moves tax revenue and licensing fees from black market to state budget; moves users from criminal legal status to consumer status with restricted but legal access. Transfers enforcement burden from criminal prosecution to regulatory compliance. Transfers property crime incentive away from users (legal prices lower than black-market markups by mid-supply, though retail may be higher; users no longer fund habits through crime). Transfers political responsibility for use-related outcomes from 'user choice' to 'regulatory design.'
% ABSENT_VOICES: Enforcement agencies built on prohibition would articulate arguments for maintaining criminal penalties; segments of public opinion viewing drug use as moral failure rather than medical/social phenomenon are structurally excluded from legalization consensus. Black market suppliers have incentive to oppose but no institutional voice in democratic framing. Prohibition-reading populations (viewing legalization as betrayal of moral order) are not represented in regulatory design; their objections are overridden via majority democratic process or technocratic authority.
% DISAPPEARANCE_RATIONALE: If the legal regulatory framework disappeared overnight, black markets would rapidly reconstitute—suppliers would return to criminal supply, users would face re-criminalization, overdose deaths from adulterants would spike, community violence would return, tax revenue would vanish, treatment integration would collapse. The constraint is constitutive of a different legal-economic order; its disappearance would force reorganization into either prohibition-enforcement or continued black-market chaos.
% FOUNDING_PROBLEM: Prohibition regime criminalizes users and enforcement fails to eliminate supply; creates black markets characterized by violence, product adulteration, overdose deaths, mass incarceration, and police-community conflict. Harm reduction and public health interventions are impossible within criminalization framework. Alternative: regulate supply as controlled market to achieve product safety, revenue generation, third-party protection, and reduced enforcement burden.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, epidemiologists, and economists outside legalization-supporting constituencies document black-market harms, overdose death trends, and mass incarceration consequences. Jurisdictions that have implemented legalization (Canada, Uruguay, some U.S. states) provide empirical attestation that the regulatory model is implementable and produces measurable reductions in overdose deaths and drug-related crime. Prohibition advocates contest that legalization increases use and normalization; this corroboration is split—evidence-based assessments from public health institutions against moral-religious objections from prohibition constituencies.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because regulatory licensing and tax extraction occur, but the primary function—eliminating black-market violence and product adulteration—delivers genuine coordination benefit to most participants. The reduction over the interval (from 0.15 projected to 0.38 observed at t=30) reflects the system maturing: initial uncertainty about regulatory design decreases as market participants adapt, compliance becomes routine, and the coordination function stabilizes. Suppression starts high (0.65 projected) because transition enforcement requires actively eliminating black-market supply and criminalizing unlicensed production; as legalization normalizes and market participants align with regulation, active suppression requirement drops to 0.22 by t=30. Theater ratio mirrors this: high at transition (0.35) as enforcement agencies justify budgets and political leaders justify the policy shift; as the market settles, performative activity drops to ~0.18 (some ongoing public education and ritual compliance inspections, but mostly functional regulation). Accessibility collapse is moderate (0.45) because users retain choice (legal purchase, use patterns) but are constrained by access restrictions (age, venue, quantity limits, product selection). Resistance stays moderate-to-high (0.52) because segments opposing legalization persist (prohibition constituencies, moral-religious objectors, some enforcement agencies); organized resistance decreases as generations of users normalize legalization, but attitudinally-rooted objection remains structural.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter seat (state regulatory authority) experiences the constraint as coordination provision with modest revenue extraction; the seat runs the regulatory apparatus, has political legitimacy for the policy, and shoulders responsibility for use-related public health outcomes. Beneficiary seats (regulated market participants, public health authorities) experience it as access and legitimacy with compliance costs they accept as necessary. Target seats (black market suppliers) experience complete market elimination and criminal prosecution—no divergence in type from the regulation's perspective, but divergence in function: what the agenda-setter sees as crime suppression, the target sees as market foreclosure via state power. Transition-cohort seats (users with criminal records) experience legal access paired with residual criminalization barriers—they benefit from the coordination function but pay the extraction cost of transition legality asymmetry. Third-party seats experience pure positive externality (crime reduction, community safety) with minimal direct cost. The engine computes per-seat types from these asymmetries: the agenda-setter and most beneficiary seats should compute as tangled_rope or rope (coordination + modest extraction); the eliminated black market seat computes as snare (pure target extraction); the transition cohort computes as tangled_rope or higher-extraction rope depending on how identity-lock is weighted.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulated market participants sit as clear beneficiaries (d~0.1-0.2): they gain market legitimacy, access to capital, predictable rules, and elimination of violent competition. Public health authorities and state regulatory bodies are institutional beneficiaries (d~0.15) collecting tax revenue and regulatory leverage while gaining legal authority. Users exiting criminalization are net beneficiaries despite some constraints (d~0.2-0.3): removal of criminal jeopardy outweighs access restrictions for most. Black market suppliers are the targeted payers (d~0.95): legal competition eliminates their market entirely. The criminalized-users transition cohort carries identity-lock (d~0.55): they gain legal status going forward but carry criminal records and employment barriers from the prior regime; their exit from criminalization is incomplete. Third parties protected from illegal markets are diffuse beneficiaries (d~0.1) gaining externality reduction without direct participation. Enforcement agencies shift from agenda-setters under prohibition to subordinate compliance roles, moving from beneficiary to payer-adjacent (excluded rather than formally payer because they retain institutional status but lose function). The measurement series tracks the suppression_requirement drop sharply (0.65→0.22) as active enforcement against black supply succeeds and legalization normalizes; this is accurate to the reading's premise that suppression is front-loaded in transition, not structural long-term.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's documented harms: mass incarceration, overdose deaths, black-market violence, treatment inaccessibility) is live in the observed period (t=10 to t=30): public health metrics show overdose decline post-legalization, incarceration reduction, violence drop in legalization jurisdictions. The constraint's founding purpose (eliminate prohibition harms via market regulation) aligns with the measured coordination function; the constraint is not mandatrophy-resolved. A potential mandatrophy pathway exists if legalization increases overall use volume to the point where new harms outweigh black-market harm reduction (the omega variable capturing this uncertainty), but observed evidence from legalization jurisdictions (Canada, Uruguay, some U.S. states) shows modest-to-no increase in use prevalence, suggesting the founding function persists. The measurement series shows extractiveness stabilizing (not rising) around 0.38 by t=30, indicating the system is not drifting toward pure extraction; theater ratio stabilizes low (~0.18), indicating continued functional activity rather than theatrical maintenance. No mandatrophy signal emerges from the structured data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_volume_increase_hypothesis,
    'Does legalization increase overall drug use volume, and if so, by how much, and does increased use erase the net harm reduction from eliminating black markets and incarceration?',
    'Long-term epidemiological tracking in legalization jurisdictions (Canada, Uruguay, U.S. states) compared to prohibition-regime controls; surveys of use prevalence, frequency, and population-level health outcomes; cost-benefit analysis integrating black-market harm reduction against use-related harms from increased volume.',
    'If legalization increases use substantially (e.g., 30%+ prevalence increase) with corresponding health harms (overdose deaths, addiction, hospital admissions, mental health impacts) that offset black-market harm reduction, the constraint''s founding problem becomes contested or shifts from ''prohibition harms are worse'' to ''trade-off between harms.'' Classification could move toward piton (atrophied function) if the regulatory apparatus persists without solving its founding problem. If use volume remains stable or increases minimally, the constraint''s founding function is confirmed and no mandatrophy emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_increase_hypothesis, empirical, 'Net public health impact of legalization when use volume increase is factored into black-market harm reduction.').

omega_variable(
    regulatory_capture_risk,
    'Do regulated market participants capture regulatory authority, converting the coordination function into pure extraction (snare-like dynamics with regulatory apparatus as the enforcement mechanism)?',
    'Longitudinal study of regulatory decisions, licensing denials, pricing pattern analysis, comparative regulation across jurisdictions; evidence of regulator-industry alignment, revolving-door hiring, preference policies favoring large participants over independent operators.',
    'If capture occurs, the constraint''s classification shifts from tangled_rope toward snare: the regulatory apparatus becomes a tool for market-participant extraction rather than coordination, suppression remains high (to enforce licensing exclusivity), and beneficiaries contract to capture-aligned market participants. If capture is resisted (strong independence, enforcement against favoritism), the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Degree to which regulatory authority is captured by regulated market participants, converting coordination into extraction.').

omega_variable(
    transition_cohort_incomplete_decriminalization,
    'For users carrying criminal records from prohibition-era enforcement, do collateral consequences and identity-lock mechanisms persist indefinitely or erode as social norms shift, and what policy interventions (record expungement, employment protections) are required to complete decriminalization?',
    'Longitudinal employment/housing outcome tracking for individuals with drug convictions; survey data on employer/landlord discrimination; analysis of expungement policies in legalization jurisdictions; qualitative research on identity reorientation post-legalization.',
    'If collateral consequences persist (high identity-lock), a subset of beneficiary seats remain substantially paying the transition cost long-term; the constraint''s beneficiary extraction is asymmetric and may be underestimated by snapshot metrics at t=30. If systematic expungement and anti-discrimination policy are implemented, the transition cohort completes its shift to full beneficiary and extractiveness drops further. This affects whether the transition cohort remains identity_locked (trapped) or becomes mobile (arbitrage-capable) in the long term.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_cohort_incomplete_decriminalization, empirical, 'Persistence of criminalization collateral consequences for users despite legalization and adequacy of policy responses.').

omega_variable(
    reading_substitution_dynamics,
    'Does the legalization reading, once institutionalized, foreclose the prohibition and harm-reduction readings within its own framework, or do all three readings coexist indefinitely as competing legitimacy claims?',
    'Political-historical analysis of legalization jurisdictions: evidence of reading foreclosure (prohibition fully delegitimized, harm reduction absorbed into legalization) versus coexistence (prohibition constituencies retain political power to threaten re-criminalization, harm reduction remains live alternative framing). Observation of shifts in institutional authority grounding (does legalization become ''natural'' authority or remain contested).',
    'If legalization forecloses prohibition within its framework, the constraint''s institutionalization is deeper and reversal less likely; if coexistence persists, the constraint remains contested and vulnerable to reading-shift via political change. This affects long-term stability and whether measurement series will show destabilization after t=30 if political conditions shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_substitution_dynamics, conceptual, 'Degree to which legalization reading achieves institutional dominance or coexists indefinitely with competing readings of the kernel.').

omega_variable(
    diffuse_externality_conversion_risk,
    'Do third parties protected from illegal markets (community safety gain) remain beneficiaries, or do regulatory failures (supply leakage to black markets, inadequate access controls leading to youth access, over-commercialization driving normalization) convert them to payers bearing unexpected costs?',
    'Tracking of secondary black-market emergence post-legalization; youth access data; community perception surveys; measurement of spillover harms (commercialization effects on neighboring jurisdictions with prohibition, normalization effects on age-restricted population).',
    'If third parties remain pure beneficiaries, the constraint''s beneficiary list holds and extracted value flows primarily to regulated market participants and state authorities. If third parties convert to payers (unexpected harms emerge), the constraint''s extraction pattern becomes more diffuse and benefits-concentration to agenda-setters increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_externality_conversion_risk, empirical, 'Stability of third-party benefit category and whether externality reduction persists or reverses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__legalization_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__legalization_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__legalization_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__legalization_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__legalization_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(subs_be_t5, projected).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__legalization_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__legalization_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__legalization_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__legalization_reading, suppression_requirement, 25, 0.24).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__legalization_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__legalization_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(subs_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The legalization_reading is one instantiation of the contested kernel substance_control_authority. Two sibling readings (prohibition_reading and harm_reduction_reading) describe different state authority claims and beneficiary/victim structures applied to the same persisting commitment: state management of drug markets. This reading assumes legalization as the superior response to prohibition's documented harms; the prohibition reading assumes criminalization is necessary for third-party protection; the harm-reduction reading minimizes state coercion and emphasizes public health interventions. All three readings share the referent (state drug policy authority) but author different ε values, beneficiary/victim sets, and classification outcomes based on their distinct premises. Constraint family links: legalization influences both prohibition (political pressure to shift policy) and harm-reduction (may absorb harm-reduction services into regulatory framework); harm-reduction coexists with legalization historically and institutionally; prohibition coexists with legalization in different jurisdictions but would foreclose legalization within a unified framework. The ε-invariance principle (DP-001) requires three separate stories because the structural assessments diverge fundamentally—legalization reads prohibition as extractive and harmful; prohibition reads legalization as abandoning third-party protection; harm reduction reads both as overstating state authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
