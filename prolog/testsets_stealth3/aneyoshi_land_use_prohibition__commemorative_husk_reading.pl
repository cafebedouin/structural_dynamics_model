% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Carved Injunction as Commemorative Husk: Below-Line Development Under Remembrance Cover
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the commemorative_husk reading of the
 *   aneyoshi_land_use_prohibition kernel: the carved injunction standing
 *   below the hamlet, erected after the 1896 and 1933 Sanriku tsunamis to
 *   warn households off the wave's reach, has decayed into a maintained
 *   memorial whose command governs no siting decision. The constraint under
 *   evaluation is the standing arrangement that decay produced: below-line
 *   shoreline land prices and builds as ordinary developable ground, seawalls
 *   and advisory hazard maps carry the protective load the injunction once
 *   carried, and the stone itself is swept, signposted, funded, and visited
 *   as heritage. Assessed by this reading's own lights the arrangement is
 *   substantially extractive: development interests, the municipal fisc, and
 *   the heritage-tourism sector capture below-line land value and
 *   reconstruction flows now, while the catastrophe the injunction was carved
 *   to prevent is scheduled onto whoever occupies the zone when the wave
 *   returns, cohorts who appear in no permit hearing. CONSTRAINT FAMILY NOTE:
 *   the colloquial label 'the Aneyoshi tsunami stone' covers two structurally
 *   distinct claims, authored as separate stories per the epsilon-invariance
 *   principle. The sibling behavioral_competence_reading authors low
 *   extraction over an enforced-prohibition referent (the injunction as live
 *   rule); this story authors high extraction (0.74) over the non-binding
 *   memorial arrangement. The readings locate the kernel's operative force
 *   differently; the disagreement is a single structural element, whether the
 *   inscription currently governs siting, and it is routed to the omega
 *   variables rather than averaged into this file.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: Primary beneficiary seat (powerful/arbitrage) - captures the regulated-versus-unregulated land value differential below the line; project capital can leave before the wave returns
 *   - below_line_newcomers: Primary target seat (moderate/trapped) - occupies seawall-marketed shoreline housing with sunk wealth; bears the scheduled inundation
 *   - municipal_government: Agenda-setting administrator with a beneficiary position (institutional/constrained) - issues the permits, maintains the memorial, channels reconstruction funds, carries the contingent disaster bill for the zone it keeps open
 *   - aneyoshi_hamlet_residents: Custodial seat (moderate/identity_locked) - keeps the stone and the annual reading; their sincere remembrance produces the cover; their homes hold the legacy high ground
 *   - coastal_tourism_sector: Secondary beneficiary (organized/mobile) - monetizes the memorial apparatus as a heritage attraction
 *   - tsunami_heritage_researchers: Analytical observer (moderate/analytical) - holds the comparative regional record of which lines held and which were built over
 *   - prospective_below_line_residents: Excluded non-agent seat (powerless/trapped) - the future cohorts who bear the heaviest scheduled cost and appear in no hearing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.74).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.42).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, snare).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Carved Injunction as Commemorative Husk: Below-Line Development Under Remembrance Cover").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '91fbf467-aa10-4bc5-8938-37d3f1b59ccb').
narrative_ontology:cs_kernel_codification('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', fixed_text).
narrative_ontology:cs_authority_grounding('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', practice).
narrative_ontology:cs_interpretation_layer_present('91fbf467-aa10-4bc5-8938-37d3f1b59ccb').
narrative_ontology:cs_reading_relation('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', aneyoshi_land_use_prohibition__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', foundational, injunction_without_force_is_memorial).
narrative_ontology:cs_axiom_status(injunction_without_force_is_memorial, holdable).
narrative_ontology:cs_axiom_grounding('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', injunction_without_force_is_memorial, empirically_contingent).
narrative_ontology:cs_axiom('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', foundational, remembrance_does_not_discharge_protective_duty).
narrative_ontology:cs_axiom_status(remembrance_does_not_discharge_protective_duty, holdable).
narrative_ontology:cs_axiom_grounding('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', remembrance_does_not_discharge_protective_duty, deontological).
narrative_ontology:cs_reference_frame('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', commemorative_monument_status).
narrative_ontology:cs_drift_state('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', post_2011_reconstruction_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('91fbf467-aa10-4bc5-8938-37d3f1b59ccb', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_government).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_tourism_sector).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_hamlet_residents).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_newcomers).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, seawall_substitution_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, self_responsibility_risk_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the heritage designation of the stone, funds its maintenance and the annual remembrance ceremony, and issues the building permits that govern the shoreline below it. Collects property tax and channels reconstruction subsidies tied to coastal rebuilding. Treats the carved line as an object of heritage interpretation rather than a rule for permit decisions; hazard maps are published in a separate register that carries no siting prohibition. It cannot leave the territory it administers, and its exposure is the future disaster-response and reconstruction bill for the zone it now permits.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_government, beneficiary).

% Acquire and build on shoreline parcels below the stone's line, where seawalls and hazard-map designations permit construction. Capture the difference between regulated and unregulated land value: because the carved line binds nothing, below-line parcels price as ordinary buildable land. Sell completed housing to buyers who assume the seawall carries the risk. Project pipelines can move to other coastlines or asset classes if local conditions turn.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, immediate, arbitrage, regional).

% Markets the stone and the remembrance circuit as heritage attractions; the memorial's national attention after 2011 increased visitor traffic. Benefits from the maintenance spending and signage that keep the site presentable. Has no interest in the carved line binding anything, since a siting prohibition would shrink the coastal facilities it operates and markets.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_tourism_sector, beneficiary,
    organized, biographical, mobile, regional).

% The households above the line who keep the stone: they sweep the site, host the annual reading of the inscription, and transmit the custodial duty to successors. Their own homes sit on the high ground their predecessors' compliance purchased. They receive heritage subsidies and the standing that comes with guardianship of a nationally known memorial. Leaving would mean abandoning a custodial role their family line has held since the carving; aging and depopulation thin the succession.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_hamlet_residents, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_hamlet_residents, agenda_setter).

% Households who have moved into shoreline housing built or rebuilt below the stone's line since the reconstruction era, most of it marketed on seawall protection. Many know the stone only as a stop on the heritage circuit and have not read its injunction as addressed to them. Their housing wealth is sunk in the zone; selling after the risk is understood means taking the loss, and the risk they carry was scheduled by decisions taken before they arrived.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_newcomers, payer,
    moderate, biographical, trapped, local).

% Academic and public-sector researchers who survey the regional stones, publish on their warning function, and advise on hazard education. They hold the full comparative record: which lines held, which were built over, which communities renewed the injunction and which let it lapse. They advocate heritage protection and land-use caution but hold no vote over permits.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, tsunami_heritage_researchers, observer,
    moderate, generational, analytical, national).

% The cohorts who will occupy the below-line zone in the coming decades: children of current residents and future buyers drawn by seawall-protected pricing. They appear in no permit hearing and no consultation; the arrangement's heaviest scheduled cost lands on them after every current decision-maker's horizon has closed.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, prospective_below_line_residents, excluded,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__commemorative_husk_reading, prospective_below_line_residents).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates two things. It coordinates coastal development expectations: builders, landowners, and the municipality share a predictable rule under which below-line parcels are buildable, permits issue, and the carved line is heritage rather than regulation, which settles what would otherwise be case-by-case siting conflict. It coordinates collective remembrance: the annual reading, the custodial succession, the school visits, the heritage circuit. What it does not coordinate is the problem the injunction was carved to solve: it leaves settlement safety to seawalls, advisory maps, and individual self-responsibility.
% TRANSFER_FUNCTION: Moves catastrophe risk from the present to the future: present seats capture below-line land value, construction revenue, tax base, and heritage traffic now, and the cost lands as mortality and asset destruction on whoever occupies the zone when the wave returns. Secondarily it moves attention and money toward the stone itself (maintenance funding, subsidies, visitor traffic) from municipal budgets and the heritage economy.
% ABSENT_VOICES: The prospective below-line cohorts are structurally absent: they cannot appear in permit hearings, hazard-map consultations, or heritage-designation proceedings, and the arrangement's heaviest scheduled cost lands on them. Renewal proponents (hazard researchers, some municipal planners, a minority of custodial households) are present in consultations but hold no vote over building permits; their objection, re-instituting the line as a binding siting rule, is deflected by seawall commitments, property-rights framing, and reconstruction economics. The unanimity of the development consensus is partly manufactured by these absences: the seats that would dissolve the arrangement are the ones it schedules costs onto.
% DISAPPEARANCE_RATIONALE: The arrangement's cover and heritage functions are load-bearing. If the maintained stone, the ceremonies, and the non-binding settlement vanished overnight, below-line development would not stop, but it would proceed nakedly, without the 'we remember' legitimation that currently keeps the zone morally open; the heritage economy (visitation, subsidies, the custodial role) would collapse; the municipality's reconstruction narrative would lose its memorial anchor; and the question of the line would be forced into the open as a live siting dispute rather than settled by commemorative default. Land values in the zone would reprice on the removal of the heritage cover alone, before any regulatory act.
% FOUNDING_PROBLEM: The 1896 and 1933 Sanriku tsunamis annihilated coastal hamlets across the region. The stone was carved to solve a land-use collective-action problem: each household building seaward captures private benefit (shorter carry, better access) while socializing tsunami risk onto the whole settlement, so no individual household could credibly commit to staying high. The carved injunction at the wave's reach was the community's commitment device: a boundary cut in stone that made the siting rule public, permanent, and hard to renegotiate in calm weather.
% FOUNDING_PROBLEM_CORROBORATION: The instrumental record corroborates the problem and its persistence entirely from outside the beneficiary set: run-up surveys of the 1896, 1933, and 2011 events (Japan Meteorological Agency and university groups) establish that the wave reaches the zone on a timescale of decades; municipal hazard maps independently designate the below-line area as inundation-prone; and the 2011 event's regional death toll is the corroborating catastrophe. No party disputes that the wave returns. Development interests dispute that the stone's line should bind, which is a dispute about the arrangement, not about the founding problem.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.74 (end-state, T=92, the current arrangement as this reading assesses it): concentrated, immediate gains (land value uplift, construction revenue, tax base, heritage traffic) captured by seats with mobile or institutional exit, against a catastrophic deferred loss carried by seats with no exit at all. The memorial apparatus is not incidental to the extraction: the maintained, venerated stone is what keeps the zone morally open, because a community that 'remembers' cannot be accused of forgetting, so the injunction's decay never has to be defended, only commemorated. Suppression 0.42 is structural, not interpersonal: the victims' lack of exit (risk scheduled before their arrival, housing wealth sunk in the zone) plus the political foreclosure of the renewal alternative (binding siting rules deflected by seawall commitments, property-rights framing, and reconstruction economics). Note the distinction from the suppression_requirement series, which traces the enforcement force the arrangement applied across the interval: post-disaster intensity decaying to ceremonial near-zero, with renewal spikes after 1960 and 2011. The scalar describes the current arrangement's coercive profile; the series traces the atrophy of the original prohibition's enforcement machinery. Theater_ratio 0.80: the warning apparatus's present activity is overwhelmingly commemorative (ceremonies, signage, school visits, heritage interpretation) where it was once overwhelmingly behavioral, when the line governed where homes went. Accessibility_collapse 0.40: alternatives have not collapsed the way a natural limit's do, since zoning law, hazard mapping, and the legal machinery for siting restriction all exist, but each is institutionally foreclosed in this jurisdiction. Resistance 0.35: researcher advocacy, heritage-preservation pressure, and the brief post-2011 renewal attempts. CYCLICAL PATTERN: the interval contains two full catastrophe-renewal-relaxation-accumulation cycles (1933 founding enforcement, interwar decay, 1960 Chile-tsunami renewal, Shōwa-era decay, 2011 Tōhoku renewal, reconstruction-era decay). The oscillation is part of the mechanism, not noise: each catastrophe briefly re-binds the line and thins the cover, and the relaxation that follows lets accumulation resume with the memorial apparatus thicker than before (more signage, more funding, more visitors), so each cycle leaves the husk heavier and the behavioral force weaker. The base_properties values were measured at T=92, in the relaxed-accumulation phase of the second cycle. All three series share one 10-point grid; the engine samples every metric at every authored point. The identity_coordination declaration is made with the cover-story risk in view: the arrangement's identity surface (custodial remembrance, heritage belonging) is genuine as practice and load-bearing as cover, and the type floor (0.08) sits far below the authored extraction, so the excess is flagged for review rather than excused by the type's complexity offset.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints. From the development seat the arrangement is barely a constraint at all: the line binds nothing, parcels price as ordinary land, and the stone is a heritage amenity that raises visitation. The husk's defining perspectival feature is that for its beneficiaries the kernel is invisible as a rule because it no longer is one. From the newcomer seat the same arrangement is an imposed catastrophe schedule they never consented to and cannot cheaply leave. The custodial seat is the subtlest: the hamlet households experience their stone-keeping as sincere filial remembrance, and it is sincere, while that same practice functions as the cover the development regime trades on. The identity lock here is relational and institutional (custodial lineage: our family keeps the stone), binding them to the site and the practice; if that identity frame broke, if the custodians reframed the injunction as addressed to the living rather than the dead, the cover would lose its most credible producers. The municipal seat straddles: administrator of the memorial and issuer of the permits, beneficiary of the tax base and carrier of the contingent disaster bill. The engine computes these divergences from the structural data; the authored claim does not adjudicate them. Coalition note: the victim seats are structurally atomized. Newcomers arrived at different times under different seawall assurances and do not constitute a constituency, and the heaviest-bearing seat (the prospective cohort) cannot coordinate at all, which is the arrangement's deepest protection against the one coalition that would dissolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. coastal_development_interests (beneficiary, arbitrage exit) derives near the full-beneficiary pole: they capture the differential and sell the risk onward before it lands. coastal_tourism_sector (beneficiary, mobile) similarly near-beneficiary. aneyoshi_hamlet_residents (beneficiary, identity_locked) derive low but off the pole, since custodial maintenance costs pull them slightly toward symmetric. below_line_newcomers (victim, trapped) derive near the full-target pole: they bear the entire scheduled loss with no exit. One directionality override is authored, for municipal_government (institutional): the derivation from its beneficiary declaration and constrained exit would place it near 0.15, but the municipality is also the residual bearer of the zone's risk, since it funds the disaster response and reconstruction its permitting schedules and faces the liability, so the override sets d to 0.30, a net beneficiary with real contingent exposure. tsunami_heritage_researchers hold the analytical seat. prospective_below_line_residents is authored with agent false and is excluded from the directionality arithmetic by design: the reading names the scheduled bearers but refuses to let unborn parties feed the computation; their position is carried in the omegas and the six-questions instead. Scope note: the arrangement's seats sit at local-to-regional scope and extraction is verifiable cheaply at this scale (permit records are public), so scope amplification should be modest; the extractiveness here lives in the structure, not in verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The injunction's mandate, keeping settlement above the wave's reach, has outlived its function, which decayed to zero while the form (the stone, the ceremonies, the heritage designation) thickened; the R5 genealogy fields carry the substance: the founding problem is live and the arrangement persists decoupled from it. The classification prevents mislabeling in both directions. Read without the beneficiary/victim structure, the husk invites the inertial-relic misreading, an atrophied form nobody profits from, maintained by habit; but the concentrated capture (development, fisc, tourism) and the identifiable scheduled victims mark it as extraction with commemorative cover, since an inertial relic characteristically has no seat that profits enough to maintain it and this arrangement has three. Read without the decay history, it invites the live-rule misreading, which is exactly what the sibling reading asserts; the measurement series is the atrophy record itself, enforcement force falling from 0.75 to 0.12 while theater rises from 0.08 to 0.80. The status-times-verdict cell is the consistent one (founding problem live, world rearranges on removal): this is not a zombie mandate, since the problem is real and the arrangement is load-bearing; it is a live problem served by a dead instrument, which is the husk reading's entire claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the aneyoshi_land_use_prohibition kernel: the commemorative_husk reading, holding that the carved injunction has decayed to symbol without behavioral force. What would the sibling behavioral_competence_reading (the injunction as live, operationally enforced land-use rule) change structurally?',
    'Land-use records resolve it directly: building permits issued below the carved line, enforcement actions (if any) against below-line siting, and municipal zoning maps'' treatment of the line. If below-line permits issue as a matter of course and no enforcement record exists, the husk reading holds; a live enforcement record would transfer the account to the sibling reading.',
    'If the sibling reading holds, epsilon collapses toward the coordination-cost floor, the victim set empties (the line holds settlement above the wave), and the classification moves toward a low-extraction coordination arrangement; the entire extractive structure authored here is conditional on the husk premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, empirical, 'Kernel contest: whether the inscription currently governs siting (sibling: live rule; this reading: commemorative husk).').

omega_variable(
    decay_reversibility_without_catastrophe,
    'Can the injunction be re-bound by administrative act, or does re-binding require a catastrophe?',
    'Comparative study of post-2011 municipal land-use restrictions across the Sanriku coast: which municipalities re-imposed binding siting limits, which relied on seawalls and advisory maps, and what happened to below-line permit volumes when reconstruction funding wound down.',
    'If only catastrophe re-binds the line, the extraction is structurally locked between events and the decay is ratcheted; if administrative renewal is feasible, the arrangement is contingent on political economy and the remedy is ordinary siting legislation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_reversibility_without_catastrophe, empirical, 'Whether the husk decay is ratcheted (catastrophe-gated renewal) or administratively reversible.').

omega_variable(
    seawall_substitution_share,
    'How much of the injunction''s lost behavioral force is attributable to seawall substitution (hard infrastructure replacing land-use discipline) versus ordinary memory attrition?',
    'Matched comparison of shoreline sites with major seawall programs against sites without, holding stone presence and remembrance-practice intensity constant: does below-line development resume faster where seawalls stand?',
    'If seawalls carry the substitution, the operative mechanism is infrastructural policy and the remedy is siting law rather than remembrance; if memory attrition dominates, the cover mechanism is cultural and heritage practice is the operative variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seawall_substitution_share, empirical, 'Attribution of the decay between infrastructural substitution and memory attrition.').

omega_variable(
    future_person_discounting,
    'The victims are largely prospective: unborn or not-yet-arrived occupants of the below-line zone. Does risk scheduled onto future persons count in the same register as extraction from present parties?',
    'Conceptual: fixed by the framework''s stance on temporal moral standing, not by data. This story''s epsilon assumes scheduled future victims count at full weight.',
    'A framework discounting future persons would lower epsilon materially and read the arrangement as ordinary development risk-taking; full-weight accounting, as authored here, is what generates the high-extraction assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_person_discounting, conceptual, 'Temporal standing of the victim class: full-weight future persons versus discounted.').

omega_variable(
    custodial_sincerity,
    'Does the cover function of the remembrance practice require custodial complicity, or does sincere commemoration alone suffice to produce it?',
    'Ethnographic: whether hamlet custodians frame the injunction as addressed to developers and newcomers or exclusively as ancestral memory; whether they have ever petitioned for binding siting rules.',
    'If custodians are unwitting instruments, their seat''s directionality sits nearer symmetric and the cover mechanism runs through institutional actors alone; if they knowingly supply cover while collecting heritage standing, their seat carries more of the arrangement''s active maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_sincerity, empirical, 'Whether the memorial cover requires custodial complicity or runs on sincere remembrance alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 92).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(aneyoshi_husk_tr_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(aneyoshi_husk_tr_t27, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 27, 0.2).
narrative_ontology:measurement(aneyoshi_husk_tr_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(aneyoshi_husk_tr_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(aneyoshi_husk_tr_t70, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 70, 0.6).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 78, 0.5).
narrative_ontology:measurement(aneyoshi_husk_tr_t81, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 81, 0.66).
narrative_ontology:measurement(aneyoshi_husk_tr_t92, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 92, 0.8).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(aneyoshi_husk_be_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(aneyoshi_husk_be_t27, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 27, 0.28).
narrative_ontology:measurement(aneyoshi_husk_be_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(aneyoshi_husk_be_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 45, 0.32).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(aneyoshi_husk_be_t70, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 70, 0.55).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 78, 0.48).
narrative_ontology:measurement(aneyoshi_husk_be_t81, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 81, 0.6).
narrative_ontology:measurement(aneyoshi_husk_be_t92, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 92, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(aneyoshi_husk_su_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(aneyoshi_husk_su_t27, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 27, 0.58).
narrative_ontology:measurement(aneyoshi_husk_su_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(aneyoshi_husk_su_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(aneyoshi_husk_su_t70, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 70, 0.18).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 78, 0.34).
narrative_ontology:measurement(aneyoshi_husk_su_t81, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 81, 0.22).
narrative_ontology:measurement(aneyoshi_husk_su_t92, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 92, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, post_2011_sanriku_reconstruction_seawall_program).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Aneyoshi tsunami stone' covers two structurally distinct claims, decomposed into a constraint family per the epsilon-invariance principle: that the carved injunction is (or was) an operative land-use rule, and that it is a commemorative object. The behavioral_competence_reading authors low extraction over an enforced-prohibition referent; this story authors high extraction over the non-binding memorial arrangement. They are linked via affects_constraints rather than merged, because a single story with a measurement parameter would let observable selection move epsilon. The upstream (behavioral) claim is the more established account of the historical enforcement era and is typically cited as evidence in heritage and disaster-education discourse, which is itself part of the cover mechanism this reading describes: the citation of the stone's protective record is what funds the memorial apparatus whose present operation carries no protective force. A second edge runs to the post-2011 Sanriku reconstruction seawall program, the infrastructure substitution that now carries much of the protective function the injunction no longer does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
