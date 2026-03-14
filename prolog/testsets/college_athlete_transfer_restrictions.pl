% ============================================================================
% CONSTRAINT STORY: college_athlete_transfer_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_college_athlete_transfer_restrictions, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: college_athlete_transfer_restrictions
 *   human_readable: NCAA Transfer Portal Restrictions and Eligibility Penalties
 *   domain: labor/education/sports
 *
 * SUMMARY:
 *   NCAA transfer restrictions form a hybrid constraint that coordinates
 *   competitive balance in collegiate athletics while simultaneously
 *   extracting value from student-athletes through mobility reduction and
 *   wage suppression. The constraint has undergone substantial structural
 *   change in the 2018-2026 period, with the creation of the transfer portal
 *   (2018), elimination of sitting-out periods (2021), and proliferation of
 *   state NIL laws (2021-2026). These changes have reduced the raw
 *   suppression baseline but have not eliminated the extractive core —
 *   institutional advantages in NIL-deal sourcing and portal access remain,
 *   and athlete identity-lock through athletic scholarships persists. The
 *   constraint exemplifies how indexical classification produces radically
 *   different readings from different structural positions: elite athletic
 *   programs experience it as pure coordination (rope), while
 *   scholarship-dependent athletes experience it as near-total extraction
 *   with identity fusion (snare). The analytical observer recognizes it as a
 *   genuine tangled rope — the coordination functions are real (without
 *   roster stability, competitive integrity degrades), but asymmetrically
 *   distributed (institutions benefit more than athletes). The theater ratio
 *   shows an interesting pattern: performative elements were high in the
 *   original restriction period (sitting-out exceptions, hardship waivers);
 *   these have been partially replaced by explicit portal mechanics and state
 *   NIL legislation, reducing the purely performative content even as the
 *   underlying institutional power dynamics remain.
 *
 * KEY AGENTS:
 *   - Student-Athletes (Primary Victims): Powerless/identity_locked — constitute their identity through athletic role; bound by scholarship dependency; face mobility restrictions that limit labor market options; experience suppression through eligibility rules and social pressure
 *   - Athletic Institutions (Primary Beneficiaries): Institutional/arbitrage — benefit from roster stability and recruiting predictability; capture athlete labor at below-market rates through scholarship caps; have power to negotiate transfer exceptions and NIL deals
 *   - NCAA Governing Body: Institutional/arbitrage — enforces rules but increasingly cannot enforce them against state NIL laws and portal mechanics; maintains performative authority over rules that are substantively undermined
 *   - Mid-Tier Programs (Secondary Agents): Moderate/constrained — experience both coordination benefits (protection from elite raid) and extraction costs (cannot recruit top portal talent); trapped between elite and non-elite tiers
 *   - State Legislatures & Legal System: Organized/mobile — created alternative verification pathways (NIL laws, O'Bannon/Alston decisions) that bypass NCAA authority; represent scaffold perspective with sunset logic
 *   - Players Association/Athlete Advocates: Organized/mobile — organizing agent pushing toward unrestricted athlete mobility; represents reform coalition with sunset clause logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(college_athlete_transfer_restrictions, 0.58).
domain_priors:suppression_score(college_athlete_transfer_restrictions, 0.68).
domain_priors:theater_ratio(college_athlete_transfer_restrictions, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(college_athlete_transfer_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(college_athlete_transfer_restrictions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(college_athlete_transfer_restrictions, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(college_athlete_transfer_restrictions, tangled_rope).
narrative_ontology:human_readable(college_athlete_transfer_restrictions, "NCAA Transfer Portal Restrictions and Eligibility Penalties").
narrative_ontology:topic_domain(college_athlete_transfer_restrictions, "labor/education/sports").

domain_priors:requires_active_enforcement(college_athlete_transfer_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(college_athlete_transfer_restrictions, athletic_institutions).
narrative_ontology:constraint_beneficiary(college_athlete_transfer_restrictions, established_coaching_staffs).
narrative_ontology:constraint_victim(college_athlete_transfer_restrictions, student_athletes).
narrative_ontology:constraint_victim(college_athlete_transfer_restrictions, competitive_balance_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT-ATHLETE (SNARE) — Trapped by identity fusion with athletic role and scholarship dependency. The athlete's identity and financial security are constituted through the scholarship. While technically mobile (could leave school), exit would mean abandoning athletic identity, losing institutional support, and likely ending collegiate athletic career. The transfer restrictions prevent lateral mobility to peer institutions without penalty. High suppression through sitting-out periods, eligibility loss, and social/team ostracism. This perspective sees pure extraction: the restriction exists to prevent athlete mobility and lock in institutional advantages.
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER PROGRAM (TANGLED ROPE) — Faces genuine coordination problem (competitive balance requires stability in roster and recruiting) but also experiences extraction. Constrained by transfer restrictions that limit ability to recruit top-transfer portal athletes, yet benefits from same restrictions preventing loss of their own players to elite programs. Asymmetric extraction: elite programs circumvent restrictions via NIL deals and portal exceptions; mid-tier programs cannot. Mixed experience of coordination and asymmetric extraction.
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE ATHLETIC PROGRAM (ROPE) — Experiences transfer restrictions as pure coordination mechanism enabling roster stability and recruiting predictability. Has arbitrage options (NCAA exemptions, NIL workarounds, direct-to-league pathways for top athletes). Sees the constraint as solving legitimate coordination problem: without roster stability rules, elite programs would raiding each other's talent continuously. Benefits from restriction architecture and has sufficient power to operate within exceptions.
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PLAYERS ASSOCIATION / REFORM COALITION (SCAFFOLD) — Organized agents (athlete advocacy groups, legal challenges, state legislators) see transfer restrictions as temporary coordination problem with sunset clause. Recent court decisions (O'Bannon, Alston, portal expansion) and state NIL legislation represent alternative verification pathways toward athlete mobility. High organizational capacity to challenge restriction enforcement. Sees this as a sunset constraint: mandatory one-year sitting-out period was eliminated in 2022-23; unlimited transfer portal access represents gradual transition to unrestricted athlete mobility. Theater is moderate (0.55) because both the old restriction and the emerging portal system contain performative elements (eligibility waivers, hardship exemptions).
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NCAA ADMINISTRATIVE SYSTEM (PITON) — Transfer rules have become substantially performative. The one-time transfer rule, sitting-out periods, and eligibility restrictions persist through institutional inertia even as the portal (created 2018, expanded 2021) and NIL legislation have undermined the enforcement mechanism. The NCAA cannot prevent athletes from using the portal, and cannot prevent state-level NIL laws from compensating transferring athletes. The restriction is maintained because institutional power and NCAA member preferences preserve it, not because it functionally accomplishes roster stability. Theater ratio (0.55) reflects that enforcement is increasingly performative: eligibility appeals, coaching-negotiated exceptions, and portal waivers are the actual decision mechanisms, not the published rules.
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, transfer restrictions coordinate multiple genuine functions: roster stability enables team culture development, recruiting predictability incentivizes coaching investment, and parity constraints balance competitive advantage. Simultaneously, the restrictions extract from athletes through mobility reduction, wage suppression (scholarship cap prevents market-rate compensation), and identity lock-in (athletic identity makes non-exit unthinkable). The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is a genuine hybrid where coordination functions are real but asymmetrically distributed (benefiting institutions more than athletes). This perspective reveals why simple policy fixes (open portal, NIL rights) do not fully resolve the constraint: the coordination function is real, so unrestricted athlete mobility risks roster chaos and competitive imbalance.
constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(college_athlete_transfer_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(college_athlete_transfer_restrictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(college_athlete_transfer_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(college_athlete_transfer_restrictions, TR),
    TR >= 0.70.

:- end_tests(college_athlete_transfer_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over the measurement interval from 0.72 to 0.58. The original restriction structure (sitting-out periods, eligibility penalties, transfer caps) created near-total wage suppression and mobility control — extractiveness was ~0.72 in the 2010-2018 period. Portal access and NIL legislation have reduced this substantially but have not eliminated it. Residual extraction remains through: (1) institutional NIL-sourcing advantages (elite programs attract better NIL deals), (2) identity lock that persists despite formal mobility rights, and (3) scholarship-based labor market segmentation (athletes cannot fully arbitrage their labor across institutions). Suppression (0.68): Moderate-high, structural + internalized. The primary mechanism is no longer enforced sitting-out periods (eliminated 2021) but is the conditional nature of scholarship (renewable annually, tied to performance and coach preference) and athlete identity-fusion with athletic role. Behavioral suppression through institutional loyalty norms and cultural expectations represents internalized component (~40% of suppression score). Structural barriers remain through eligibility restrictions and institutional-level transfer portal gatekeeping. Theater ratio (0.55): Moderate and rising. Early-period theater was high (sitting-out exceptions, hardship waivers, vague eligibility criteria) — the rules created performative gates that institutions could navigate. Post-portal theater has shifted: explicit portal mechanics and state NIL legislation reduced the opacity, but new performative elements emerged (NIL eligibility determinations, coaching negotiation of portal exceptions). Theater is rising toward equilibrium because institutional actors are adapting their performative strategies to the new landscape.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence driven by the agent's structural position relative to the extraction flow. The elite athletic program (Rope perspective) genuinely solves a coordination problem — unlimited athlete mobility would prevent program building and squad stability. The student-athlete (Snare perspective) sees pure extraction because the mobility restrictions are experienced as identity-constituted and inescapable. The mid-tier program (Tangled Rope) sees mixed coordination (protection from elite raiding) and asymmetric extraction (unable to recruit elite portal talent). The scaffold perspective (reform coalition) sees the constraint as temporary, with real sunset mechanisms (portal, NIL laws, court decisions) actively degrading the restriction's enforcement capacity. The piton perspective (NCAA administration) sees its own rules as increasingly performative and inert — the NCAA's authority to enforce transfer restrictions is substantively undermined by state-level NIL laws and portal mechanics, yet the institution maintains the rules through inertia and member preferences. The analytical observer recognizes the genuine coordination function (roster stability enables team culture) alongside the genuine extraction (wage suppression through scholarship caps), producing the tangled rope classification. This perspectival gap is the diagnostic signature that transfer restrictions are not a pure coordination problem (which would show rope across most perspectives) nor a pure extraction scheme (which would show snare across powerless and moderate perspectives). Instead, the gap reveals that the constraint's function varies by institutional tier: it coordinates for elite programs while extracting from athletes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's relationship to the extraction flow and their structural exit capacity. Student-athletes are victims (high d, approaching 1.0 due to identity_locked exit status) with trapped/identity_locked exit modulation. The engine computes d~0.89 for identity_locked victims, producing f(d)~1.28, which amplifies their experienced chi substantially. Athletic institutions are beneficiaries (low d, ~0.15 from institutional/arbitrage position), producing f(d)~-0.01, resulting in negative effective extraction (they experience coordination benefit). Mid-tier programs are both partial beneficiary (protected from elite raiding) and partial victim (constrained from recruiting elite talent), producing intermediate d~0.50, f(d)~0.65, moderate experienced chi. The NCAA/piton perspective is institutional/arbitrage but experiences diminished power due to substantive rule undermining, which the override mechanism can capture by increasing d from canonical 0.00 to ~0.30, reflecting the constraint's actual enforcement position. No explicit directionality overrides are necessary — the structural data (beneficiary/victim + exit options) derives appropriate d values for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognition that transfer restrictions serve genuine coordination functions (competitive balance, roster stability, recruiting predictability) that are asymmetrically distributed. The constraint is not mislabeled as pure extraction (snare) when it actually coordinates — the tangled rope classification correctly captures that BOTH functions are real. However, the asymmetry matters: institutions derive disproportionate coordination benefit, while athletes experience coordination requirements as extraction. The analytical observer's tangled rope classification prevents false natural law misidentification (mountain) while also preventing false pure-extraction misidentification (snare). The constraint is legitimately hybrid, not contingently mislabeled. The mandatrophy analysis also reveals that policy solutions targeting one component (e.g., open portal) do not fully resolve the constraint because they address suppression (structural barriers to transfer) but leave extraction intact (wage suppression through scholarship caps, identity-lock through athletic culture). Complete resolution would require addressing both the coordination function (how to enable roster stability without transfer restrictions) and the asymmetric extraction (how to allow athletes market-rate compensation). The scaffold perspective captures this: sunrise of NIL + portal addresses suppression reduction, but sunset requires solving the coordination problem (league-style franchising, professional minor leagues, international models) that currently depends on transfer restrictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_persistence,
    'Does the athlete''s identity lock survive portal access and NIL compensation, or does athlete mobility fundamentally dissolve the lock?',
    'Longitudinal analysis of athlete behavior post-portal access (2021-2026 cohorts): Do athletes with unrestricted mobility still exhibit identity fusion with original institution? Do transfer rates saturate or continue rising? Do athlete narratives shift from ''locked in'' to ''choosing my path''?',
    'If identity lock persists despite mobility: the constraint is deeper than transfer rules — it is constituted through sport culture and athletic identity itself. Classification remains snare even with open portal. If identity lock dissolves: transfer rules were the primary mechanism, and removal resolves the extractive core. Classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity lock persists after portal and NIL access').

omega_variable(
    competitive_balance_necessity,
    'Are transfer restrictions genuinely necessary to maintain competitive balance, or is this a cover story for institutional power preservation?',
    'Comparative analysis of parity indices: (1) Pre-portal (2000-2018) vs post-portal (2021-2026) competitive balance metrics (Herfindahl index of championship concentration, mid-tier team playoff appearance rates). (2) International sports leagues without transfer restrictions (European soccer, cricket franchises) — do they show lower competitive balance? (3) Economic modeling of league stability under free-agent athlete mobility.',
    'If restrictions are genuinely necessary: transfer rules solve a real coordination problem, justifying high suppression. Tangled Rope classification is correct. If cover story: restrictions are pure extraction with institutional-generated narrative. Classification shifts toward Snare for most non-elite perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competitive_balance_necessity, empirical, 'Whether transfer restrictions are necessary for competitive balance').

omega_variable(
    nil_compensation_sufficiency,
    'Do NIL rights and portal access constitute meaningful compensation for athletes, or are they partial substitutes that leave core extraction mechanisms intact?',
    'Empirical measurement of athlete lifetime earnings and career trajectory: (1) Total athlete compensation (scholarship + NIL + post-collegiate earnings) by transfer status and tier. (2) Career path analysis: do transferring athletes achieve higher professional earnings than non-transferring athletes in same draft cohorts? (3) Comparison of athlete earnings under NCAA model vs pure free-agent model (Australian rules, international professional leagues).',
    'If NIL + portal = meaningful compensation: extraction pressure is substantially reduced; constraint may shift from Snare to Rope from athlete perspective. If partial substitute: core wage suppression persists; athletes trade identity lock for some mobility but remain economically extractive. Constraint remains Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nil_compensation_sufficiency, empirical, 'Whether NIL and portal access provide meaningful athlete compensation').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.68) structural (enforced restrictions) or internalized (athlete-internalized norms about loyalty, institutional commitment, ''respecting the program'')?',
    'Qualitative and behavioral analysis: (1) Athlete interviews about transfer decisions — what barriers are cited as most significant? (2) Comparison of portal transfer rates when structural barriers are removed (one-time transfer eligibility) vs baseline. (3) Exit patterns for athletes with equivalent structural mobility but different identity fusion (e.g., grad student vs undergrad athletes).',
    'If structural: removing transfer rules substantially reduces suppression. If internalized: suppression persists as athlete-carried constraint even after rule changes. Affects therapeutic interpretation — whether policy change solves the problem or merely reveals deeper identity-constituted extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized norm').

omega_variable(
    elite_program_arbitrage_boundary,
    'What institutional size/tier is the threshold where NCAA exemptions and NIL workarounds become available, and how does this threshold shift the perspectival classification?',
    'Mapping of NCAA waiver grants, NIL deal availability, and portal exception rates by institutional revenue tier (elite programs, Power 5 mid-tier, Group of 5, Division 2). Identify threshold where institutional power translates to enforcement exceptions.',
    'If threshold is sharp: creates two-tier system (elite arbitrage, non-elite trapped). Enhances Snare classification for non-elite athletes, Rope for elite. If threshold is fuzzy: entire spectrum shows Tangled Rope with graduated asymmetry. Affects network decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_program_arbitrage_boundary, empirical, 'Threshold for institutional power to access NCAA exemptions and NIL workarounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(college_athlete_transfer_restrictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, college_athlete_transfer_restrictions, theater_ratio, 0, 0.35).
narrative_ontology:measurement(coll_tr_t5, college_athlete_transfer_restrictions, theater_ratio, 5, 0.48).
narrative_ontology:measurement(coll_tr_t10, college_athlete_transfer_restrictions, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, college_athlete_transfer_restrictions, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(coll_be_t5, college_athlete_transfer_restrictions, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(coll_be_t10, college_athlete_transfer_restrictions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(college_athlete_transfer_restrictions, resource_allocation).
narrative_ontology:affects_constraint(college_athlete_transfer_restrictions, ncaa_athlete_compensation_cap).
narrative_ontology:affects_constraint(college_athlete_transfer_restrictions, college_athletic_labor_market_segmentation).

% DUAL FORMULATION NOTE:
% The transfer restrictions form a constraint family with athlete wage suppression (scholarship caps) and athletic labor market segmentation. Transfer restrictions address mobility; wage suppression addresses compensation; market segmentation addresses entire labor market structure. Each story has different ε: transfer restrictions alone ~0.58 (Tangled Rope), wage suppression alone ~0.72 (Snare), market segmentation ~0.80 (Snare). Network links capture that addressing transfer restrictions without addressing wage suppression will not resolve athlete extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(college_athlete_transfer_restrictions, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
