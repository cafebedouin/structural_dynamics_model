% ============================================================================
% CONSTRAINT STORY: parlements_inner_container_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parlements_inner_container_authority, []).

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
 *   constraint_id: parlements_inner_container_authority
 *   human_readable: Parlements as Inner-Container Operational Authority During Estates-General Dormancy
 *   domain: french_history/judicial_authority
 *
 * SUMMARY:
 *   Between the last convocation of the Estates-General in 1614 and its
 *   reconvocation in 1789, France operated under a system where formal
 *   authority lay with the crown but operational authority gradually shifted
 *   to the thirteen regional parlements (sovereign courts, not legislative
 *   bodies in the modern sense). The Parlement of Paris, in particular,
 *   acquired the effective power to block royal legislation through refusal
 *   to register edicts. This constraint exemplifies the Deferential Realism
 *   prediction that dormant outer containers (the Estates-General)
 *   structurally enable inner containers (parlements) to absorb their
 *   operational authority. The parlements performed the function the
 *   Estates-General had abandoned: representing noble and bourgeois
 *   interests, remonstrating against royal overreach, controlling fiscal
 *   extraction, and maintaining legal stability. However, they did so while
 *   extracting their own rents through venal office, judicial fees, and
 *   registration concessions. The constraint is therefore neither pure
 *   coordination (Rope) nor pure extraction (Snare) but a hybrid (Tangled
 *   Rope): parlements genuinely coordinated governance and provided checks on
 *   crown absolutism, but they simultaneously extracted wealth and
 *   concessions from the crown and, more significantly, from the regional
 *   subjects they governed. The operational authority inversion is real and
 *   consequential — parlements were formally subordinate to the crown yet
 *   operationally equal or superior in matters requiring registration. The
 *   175-year interval encompasses the full lifecycle: initial authority
 *   vacuum after 1614, gradual parlement expansion of remonstrance and veto
 *   power (1650s-1750s), peak parlement dominance (1750-1774), Maupeou
 *   reforms attempting to reassert crown control (1768-1774), restoration of
 *   parlement power (1774-1789), and final collapse when fiscal crisis and
 *   ideological delegitimacy converged in 1789.
 *
 * KEY AGENTS:
 *   - Parlement Corporations (magistrate class): Primary beneficiaries (institutional/arbitrage) — capture venal office wealth, judicial fees, registration concessions, and veto power over legislation. Experience constraint as legitimate coordination of jurisprudence and resistance to royal overreach.
 *   - The Monarchy: Primary victim (organized/constrained) — cannot legislate without parlement registration; forced to negotiate, make fiscal concessions, and accept reduced tax collection. Also constrained because dissolution of parlements would trigger fiscal and legitimacy collapse.
 *   - The Crown's Legislative Will: Structural victim (powerless/trapped) — any new edict requires submission to parlement; refusal is a veto that cannot be overridden without formal breach or military occupation.
 *   - Royal Fiscal Capacity: Victim (powerless/trapped) — parlement refusal to register fiscal edicts directly reduces crown revenue; registration concessions (exemptions, debt forgiveness) further reduce fiscal extraction.
 *   - Regional Commoners and Peasants: Victims (powerless/trapped) — parlement law provides some protection against arbitrary royal extraction but does not protect against parlement-class extraction; cannot exit jurisdiction or appeal beyond parlement.
 *   - The Magistrate-Client Networks: Beneficiaries (moderate/constrained) — individual magistrates benefit from parlement membership and venal office but are constrained by parlement corporation's tactics and must participate in costly remonstrance campaigns.
 *   - The Estates-General (Dormant Outer Container): Latent structural force (powerful/mobile) — exists as a potential reform mechanism but dormant; reconvocation threatened parlement authority, so parlements opposed revival throughout the interval.
 *   - The Formal Hierarchy (Institutional Theater): Degraded function (institutional/arbitrage) — persists as legal fiction that crown is supreme while operational reality inverts hierarchy; increasingly theatrical and unsustainable by 18th century.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parlements_inner_container_authority, 0.52).
domain_priors:suppression_score(parlements_inner_container_authority, 0.48).
domain_priors:theater_ratio(parlements_inner_container_authority, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parlements_inner_container_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(parlements_inner_container_authority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(parlements_inner_container_authority, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parlements_inner_container_authority, tangled_rope).
narrative_ontology:human_readable(parlements_inner_container_authority, "Parlements as Inner-Container Operational Authority During Estates-General Dormancy").
narrative_ontology:topic_domain(parlements_inner_container_authority, "french_history/judicial_authority").

domain_priors:requires_active_enforcement(parlements_inner_container_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parlements_inner_container_authority, parlement_corporations).
narrative_ontology:constraint_beneficiary(parlements_inner_container_authority, magistrate_class).
narrative_ontology:constraint_victim(parlements_inner_container_authority, royal_legislative_prerogative).
narrative_ontology:constraint_victim(parlements_inner_container_authority, crown_revenue_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROWN'S LEGISLATIVE WILL (SNARE) — The monarchy cannot bypass parlement registration without formal breach. Each edict requires submission; refusal to register is a veto that cannot be overridden without military action or wholesale replacement of magistrates. The crown faces maximum extraction — parlements extract concessions (payment of debts, reduction of fiscal demands, exemption grants) in exchange for registration. No coordination benefit; pure blocking power weaponized for rent-seeking.
constraint_indexing:constraint_classification(parlements_inner_container_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MONARCHY AS INSTITUTIONAL ACTOR (TANGLED ROPE) — The crown experiences both genuine coordination (parlements provide legal legitimacy, appeal jurisdiction, and enforcement capacity in their regions) and asymmetric extraction (registration refusal yields concessions, parlements capture revenue, magistrate positions are venal and self-perpetuating). The constraint requires active enforcement — the crown must continuously negotiate, threaten judicial removal, or mobilize force. Neither full beneficiary nor full victim; constrained by the necessity of parlement cooperation for legitimacy and tax collection.
constraint_indexing:constraint_classification(parlements_inner_container_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLEMENT CORPORATIONS (ROPE) — Parlements experience the constraint as legitimate coordination. They coordinate jurisprudence across their jurisdictions, protect property rights (especially parlement members' property), resist arbitrary royal fiscal extraction, and maintain legal stability. The registration power is both a coordination function (ensuring edict legitimacy) and a beneficiary position (they extract concessions and maintain institutional autonomy). From their internal perspective, the constraint is pure coordination — they are solving the governance problem that the dormant Estates-General no longer solves. They have arbitrage options: a parlement can threaten passive resistance (refusing to register) or active appeal to magistrate solidarity (collective remonstrance).
constraint_indexing:constraint_classification(parlements_inner_container_authority, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL MAGISTRATE-CLIENTS (TANGLED ROPE) — Individual magistrates benefit from parlement membership (venal office, social status, appeal jurisdiction) but are constrained by the parlement corporation's political tactics. They experience both coordination (parlement law protects their local interests and property) and extraction (they must participate in costly remonstrance campaigns, face crown retribution when parlement resists, and cannot exit parlement office without heavy financial loss). They benefit from parlement's arbitrage options but bear some cost when those options are exercised.
constraint_indexing:constraint_classification(parlements_inner_container_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGIONAL PEASANTRY AND COMMONERS (SNARE) — Parlements coordinate law but extract enforcement costs and judicial fees. Commoners cannot appeal beyond parlement and cannot exit the jurisdiction. They experience parlement authority as extraction-dominant: judicial fees, tax collection (which parlements enforce via royal authorization), and arbitrary enforcement by magistrate clients who use parlement judicial backing to extract local rents. No meaningful exit option or coalition power. The parlement's resistance to royal fiscal demands sometimes reduces crown tax burden, but magistrate-class extraction typically fills the gap.
constraint_indexing:constraint_classification(parlements_inner_container_authority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ESTATES-GENERAL AS LATENT OUTER CONTAINER (SCAFFOLD) — During the 175-year dormancy, the Estates-General exists as a potential alternative authority structure. Its eventual reconvocation (1789) demonstrates that parlement authority is temporary and contingent on the outer container's continued dormancy. Parlement dominance is scaffolding that can be removed when the Estates-General reforms itself. From the latent perspective, the constraint is low-extraction temporary support: parlements coordinate governance during the gap but with an implicit sunset. The outer container has mobile options — it can reconvene and reassert authority — and this latent threat structures the entire parlement-monarchy negotiation.
constraint_indexing:constraint_classification(parlements_inner_container_authority, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: VESTIGIAL FORMAL HIERARCHY (PITON) — The formal institutional order places parlements as subordinate to the crown, yet operational authority has inverted. The theater of submission persists: parlements formally register edicts while operationally blocking them; they perform deference to royal prerogative while exercising veto. By the 18th century, the formal hierarchy is increasingly theatrical — everyone knows parlements are structural equals or superiors to the crown in matters of registration. The constraint persists through institutional inertia: the legal fiction of royal supremacy remains unstated but contested, and no one can formally admit the inversion without wholesale reformation of the state. Theater rises as operational divergence from formal structure increases.
constraint_indexing:constraint_classification(parlements_inner_container_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective focused on nested-container dynamics, the constraint appears as an immutable feature of distributed authority systems: when an outer container (Estates-General) stops functioning, inner containers (parlements) necessarily absorb its operational authority. This is a structural inevitability in any regime where authority is formally hierarchical but operationally distributed. No design choice could have prevented parlement authority expansion while the Estates-General remained dormant. However, the structural data (clear beneficiaries in parlement corporations, victims in crown prerogative and regional commoners) suggests this is a false summit — the constraint is not inherent to nested systems but rather a contingent institutional arrangement that beneficiaries have leveraged to extract concessions.
constraint_indexing:constraint_classification(parlements_inner_container_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parlements_inner_container_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parlements_inner_container_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parlements_inner_container_authority, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parlements_inner_container_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parlements_inner_container_authority, TR),
    TR >= 0.70.

:- end_tests(parlements_inner_container_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts substantial wealth from the crown through registration concessions and from regional subjects through judicial fees and magistrate-class rents. However, it is not as severe as Snare-level extraction (≥0.66) because parlements provide genuine coordination functions (legal stability, property protection, check on arbitrary royal power) that have real value. The trajectory shows extraction accumulation over the interval: 0.28 at start (authority void, parlements still asserting claims) to 0.63 at end (near-total operational dominance). The increase reflects parlements systematizing their veto power and extracting ever-larger concessions as the crown becomes increasingly fiscally desperate. Suppression (0.48): Moderate. Parlement authority suppresses alternatives (no other institution can register edicts), but suppression is not total (crown can use Lit de Justice or lettres de cachet for emergency bypass). Subjects cannot exit parlement jurisdiction, but they have limited appeal mechanisms. Magistrates can exit parlement office but face heavy financial loss. Theater ratio (0.35): Low-moderate. The formal hierarchy (crown supreme) is increasingly theatrical, but the registration function itself is not performative — it is structurally consequential. By the 18th century, everyone knows that parlement registration is a veto, not a rubber stamp, but the legal fiction persists. Theater increases gradually (0.15→0.35) as the operational inversion becomes more obvious and the formal hierarchy must work harder to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divergence driven by structural position. Parlement corporations experience Rope (pure coordination) because they are the architects of their own authority and genuinely coordinate jurisprudence and checks on crown power. The crown experiences Tangled Rope (mixed coordination and extraction) because parlements both provide legitimate governance and extract registration concessions. Regional commoners and the crown's legislative will experience Snare (pure extraction) because they are trapped and cannot exit or organize. The latent Estates-General experiences Scaffold (temporary authority) because parlement dominance is contingent on Estates-General dormancy and would reverse upon reconvocation. The formal hierarchy experiences Piton (degraded ritual) because the legal fiction of crown supremacy persists while operational reality inverts it. The analytical observer risks seeing Mountain (structural inevitability of nested-container authority absorption) but the structural data (clear beneficiaries, identifiable extraction mechanism, contingent institutional arrangements) suggests a false summit: the constraint is designed and leveraged by beneficiaries rather than inherent to authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position relative to this specific constraint. Parlement corporations are beneficiaries with arbitrage options (they can threaten registration refusal or judicial defection) — low d, negative effective extraction chi from their perspective (they experience net benefit). The crown is a victim with constrained options (cannot dissolve parlements without regime instability) — moderate-high d, producing high chi (they experience significant extraction). Regional commoners are trapped victims with no exit — highest d, maximum experienced extraction (Snare perspective). The magistrate-client class occupies the middle: beneficiaries (they gain office and prestige) but constrained (they cannot freely exit parlement or control its political tactics) — moderate d. The latent Estates-General has high structural power (mobile options, could reconvene) but dormancy means it is not presently exercising that power — moderate-low d during dormancy, but this shifts dramatically once reconvocation occurs. The formal hierarchy is a vestigial institutional structure that persists through inertia, not through active enforcement — arbitrage options are theoretically available (reform the hierarchy) but politically infeasible (no one can openly declare the inversion). This produces the piton classification: low experienced extraction because the constraint is not being enforced, but high theater because the formal fiction must be maintained.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint IS genuinely a Tangled Rope hybrid: parlements coordinate governance (register legitimate legislation, provide appeal jurisdiction, stabilize property rights, check arbitrary royal power) while simultaneously extracting (through venal office rents, judicial fees, and registration concessions). The classification is not ambiguous — it is dual. What resolves the mandatrophy tension is that different observers experience different ratios of coordination to extraction. Parlement corporations weight coordination heavily and experience Rope. The crown weights extraction heavily and experiences Tangled Rope. Commoners weight extraction almost exclusively and experience Snare. The formal hierarchy persists as theatrical coordination (Piton) because everyone has incentives to maintain the legal fiction that parlements are subordinate even though operational reality has inverted. The analytical observer risks seeing Mountain (structural inevitability) but this is a false summit — the constraint is contingent on parlement corporations choosing to leverage their vacant-container opportunity. A different choice (crown reconvoking Estates-General sooner, dissolving parlements despite cost, creating alternative registration bodies) would have produced different structures. The constraint is therefore not immutable — it is leveraged institutional arrangement masquerading as structural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    registration_power_nature,
    'Is parlement registration authority a necessary coordination function or a captured veto gate?',
    'Historical analysis of edict registration patterns: rates of refusal, timing of remonstrance relative to implementation, correlation between registration refusal and subsequent crown concessions (financial, regulatory, or personnel). Comparison with periods when crown bypassed registration (Lit de Justice, lettres de cachet) to identify which function was actually essential.',
    'If necessary coordination: rope classification confirmed for parlement perspective. If captured veto: snare classification for crown perspective confirmed. If both simultaneously: tangled_rope for both is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(registration_power_nature, empirical, 'Whether registration is coordination function or captured veto').

omega_variable(
    crown_exit_capacity,
    'Could the crown have dissolved or reformed parlements to eliminate their veto power without triggering total regime collapse?',
    'Analysis of crown attempts at parlement reform (Maupeou, d''Aguesseau reorganizations): outcomes, duration, resistance mechanisms, whether reforms stuck or were reversed post-reform. Assessment of whether crown had military/fiscal capacity to maintain reformed parlement against magistrate opposition.',
    'If exit was truly infeasible: crown was genuinely trapped (mountain from crown perspective). If exit required only political will and military commitment: crown chose constrained negotiation over forced dissolution (tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_exit_capacity, empirical, 'Whether crown had exit capacity from parlement authority').

omega_variable(
    beneficiary_capture_scope,
    'Did parlement authority extraction primarily benefit the magistrate class or the broader regional societies they governed?',
    'Distributional analysis: tax records showing parlement fee vs crown tax revenue; property records showing whether magistrate class captured most parlement-derived wealth; regional economic data comparing areas with powerful vs weak parlements; peasant records and commoner grievance documentation.',
    'If extraction concentrated in magistrate class: parlement provides no coordination benefit to broader subjects (snare from peasant perspective confirmed). If extraction was distributed or parlements reduced crown fiscal extraction in ways that benefited regions: some coordination function exists (tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_scope, empirical, 'Scope of parlement beneficiary class and extraction distribution').

omega_variable(
    estates_general_inertia,
    'Why did the Estates-General not reconvene to reassert authority during the 175-year dormancy?',
    'Political economy of Estates-General revival: cost-benefit analysis of reconvocation vs status quo for crown; incentive structures of parlement magistrates (would they have supported Estates-General to dilute their own authority?); external shocks (wars, fiscal crises) that did or did not trigger reconvocation attempts.',
    'If dormancy was contingent (crown could have reconvoked but chose status quo): scaffold sunset mechanism was latent and real. If dormancy was driven by structural incentives (crown benefited from dealing with fragmented parlements rather than unified Estates): outer container dormancy itself may be a constraint with its own extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(estates_general_inertia, empirical, 'Causal mechanisms behind 175-year Estates-General dormancy').

omega_variable(
    operational_vs_formal_hierarchy_sustainability,
    'How long could the system sustain the divergence between formal hierarchy (crown supreme) and operational hierarchy (parlements supreme in registration) before legitimacy collapsed?',
    'Timeline analysis of institutional stress indicators: frequency and scale of remonstrance, escalation of crown threats (dissolution, Lit de Justice, lettres de cachet), magistrate defections, external pressure (nobility, church, commons) on both crown and parlements. Correlation with 1789 collapse.',
    'If divergence was unsustainable (inherent expiration): mountain classification has merit — the constraint naturally degrades. If divergence was indefinitely sustainable until external shock (1789 fiscal collapse): system was in equilibrium that could have persisted; constraint is not naturally expiring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_formal_hierarchy_sustainability, empirical, 'Sustainability timeline of formal/operational hierarchy divergence').

omega_variable(
    false_summit_false_positive,
    'Is parlement authority expansion a false summit (naturalized extraction) or a genuine structural inevitability of dormant outer containers?',
    'Comparative institutional analysis: do other nested-container systems with dormant outer containers show identical inner-container authority absorption? (Medieval estates, city councils during parliament suspension, provincial councils during federal dormancy.) If pattern is universal: mountain classification. If parlement authority expansion was contingent on specific choices (venality, Gallican church independence, regional fragmentation): false summit.',
    'If mountain: the constraint cannot be reformed short of outer-container reactivation. If false summit: parlement authority is designed institutional arrangement that could be reformed via crown political will.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_false_positive, conceptual, 'Whether parlements instantiate universal nested-container logic or contingent French institutional choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parlements_inner_container_authority, 0, 175).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parl_tr_t0, parlements_inner_container_authority, theater_ratio, 0, 0.15).
narrative_ontology:measurement(parl_tr_t40, parlements_inner_container_authority, theater_ratio, 40, 0.22).
narrative_ontology:measurement(parl_tr_t80, parlements_inner_container_authority, theater_ratio, 80, 0.3).
narrative_ontology:measurement(parl_tr_t120, parlements_inner_container_authority, theater_ratio, 120, 0.33).
narrative_ontology:measurement(parl_tr_t160, parlements_inner_container_authority, theater_ratio, 160, 0.35).
narrative_ontology:measurement(parl_tr_t175, parlements_inner_container_authority, theater_ratio, 175, 0.35).

% Extraction over time
narrative_ontology:measurement(parl_be_t0, parlements_inner_container_authority, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(parl_be_t40, parlements_inner_container_authority, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(parl_be_t80, parlements_inner_container_authority, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(parl_be_t120, parlements_inner_container_authority, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(parl_be_t160, parlements_inner_container_authority, base_extractiveness, 160, 0.61).
narrative_ontology:measurement(parl_be_t175, parlements_inner_container_authority, base_extractiveness, 175, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parlements_inner_container_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(parlements_inner_container_authority, estates_general_fiscal_authority).
narrative_ontology:affects_constraint(parlements_inner_container_authority, noble_exemptions_fiscal_extraction).
narrative_ontology:affects_constraint(parlements_inner_container_authority, parlement_venal_office_market).
narrative_ontology:affects_constraint(parlements_inner_container_authority, lit_de_justice_crown_bypass).

% DUAL FORMULATION NOTE:
% The parlement authority constraint decomposes into multiple structurally distinct claims: (1) parlement registration as coordination function (ε≈0.15, Rope), (2) parlement registration as crown extraction veto (ε≈0.60, Snare from crown perspective), (3) parlement magistrate-class rents and fees (ε≈0.55, Snare from commoner perspective), (4) formal hierarchy theater (ε≈0.20, Piton). This story models the hybrid (Tangled Rope, ε=0.52) integrating all four. The downstream constraints (Lit de Justice as crown bypass, venal office as parlement self-perpetuation, Estates-General as outer-container alternative, noble exemptions as coordination failure) should be modeled as separate stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parlements_inner_container_authority, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
