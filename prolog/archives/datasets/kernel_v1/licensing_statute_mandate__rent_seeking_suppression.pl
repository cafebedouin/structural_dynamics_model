% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Licensing Requirements as Rent-Seeking Suppression
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory occupational licensing creates artificial scarcity in labor
 *   supply by requiring government-mandated credentials (degrees,
 *   certificates, continuing education, license fees) to enter regulated
 *   occupations. This constraint exhibits structural extraction: incumbents
 *   benefit from reduced competition and elevated prices; entrants and
 *   consumers bear suppression costs. The constraint is one reading of a
 *   contested kernel—the statutory mandate itself. This reading interprets
 *   the mandate as primarily a rent-seeking mechanism masked by public safety
 *   justification (the rent_seeking_suppression reading). Sibling readings
 *   frame the same statute as genuine public safety coordination or as a
 *   graduated access filter that serves both functions. The extractiveness
 *   has increased from ~0.35 (1975) to ~0.58 (2025) as credential inflation
 *   has accelerated independent of occupational risk changes. Theater ratio
 *   has risen as continuing education and credential maintenance requirements
 *   have become increasingly disconnected from actual public safety
 *   improvements. Suppression has remained high and stable because entry
 *   barriers are legally enforced—entrants cannot exit without either bearing
 *   full credential costs or abandoning the profession entirely.
 *
 * KEY AGENTS:
 *   - Incumbent Practitioners: Primary beneficiary (institutional/arbitrage) — benefit from reduced competition, maintained pricing power, and professional prestige; largely control licensing board composition
 *   - Labor Market Entrants: Primary victim (powerless/trapped) — face mandatory, legally enforced credential requirements with no exit; must either acquire costly credentials, relocate, or abandon the profession
 *   - Consumers: Secondary victim (moderate/constrained) — bear costs through elevated prices and reduced access; face suppression through inability to organize challenge to boards
 *   - Licensing Boards: Institutional actor with hybrid position (institutional/constrained) — coordinate genuine public safety standards and simultaneously extract rent for incumbents; board members typically drawn from incumbent practitioners
 *   - Alternative Credential Holders (Apprentices, Reciprocal Practitioners, Military-Trained): Victims (moderate/trapped) — legally excluded from practice despite demonstrating competence through alternative pathways
 *   - Analytical Observer: Sees pure extraction mechanism (analytical/analytical) — public safety can be achieved through transparent testing and liability without artificial scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.58).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Licensing Requirements as Rent-Seeking Suppression").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '81c86094-3b02-49a8-b81a-9c98636ebf51').
narrative_ontology:cs_kernel_codification('81c86094-3b02-49a8-b81a-9c98636ebf51', formalized).
narrative_ontology:cs_authority_grounding('81c86094-3b02-49a8-b81a-9c98636ebf51', extraction).
narrative_ontology:cs_interpretation_layer_present('81c86094-3b02-49a8-b81a-9c98636ebf51').
narrative_ontology:cs_reading_relation('81c86094-3b02-49a8-b81a-9c98636ebf51', licensing_statute_mandate__public_safety_coordination, influences).
narrative_ontology:cs_reading_relation('81c86094-3b02-49a8-b81a-9c98636ebf51', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('81c86094-3b02-49a8-b81a-9c98636ebf51', foundational, credentialing_surplus_extraction_primary).
narrative_ontology:cs_axiom_status(credentialing_surplus_extraction_primary, holdable).
narrative_ontology:cs_axiom_grounding('81c86094-3b02-49a8-b81a-9c98636ebf51', credentialing_surplus_extraction_primary, empirically_contingent).
narrative_ontology:cs_axiom('81c86094-3b02-49a8-b81a-9c98636ebf51', secondary, incumbent_capture_inevitable).
narrative_ontology:cs_axiom_status(incumbent_capture_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('81c86094-3b02-49a8-b81a-9c98636ebf51', incumbent_capture_inevitable, instrumental).
narrative_ontology:cs_reference_frame('81c86094-3b02-49a8-b81a-9c98636ebf51', occupational_competition_unconstrained).
narrative_ontology:cs_drift_state('81c86094-3b02-49a8-b81a-9c98636ebf51', contemporary_credential_inflation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('81c86094-3b02-49a8-b81a-9c98636ebf51', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_via_price_elevation).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, alternative_credential_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR MARKET ENTRANT (SNARE) — Faces mandatory, government-enforced credentialing requirements with no meaningful exit. Entry is legally impossible without statutory compliance. Suppression is complete: the entrant must either (a) spend years acquiring the mandated credential at substantial cost, (b) exit the profession entirely, or (c) operate illegally. The licensing statute creates artificial scarcity; extraction flows entirely toward incumbents.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER (SNARE) — Constrained by reduced supply and elevated prices created by the artificial scarcity. Can theoretically exit by not purchasing the service, but this is often impractical (occupational licensing for healthcare, legal services, plumbing, electrical work). The consumer bears the cost of the rent extraction through both higher prices and reduced access. Suppression is high because consumers cannot organize to challenge licensing boards.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT PRACTITIONER (ROPE) — Perceives the licensing statute as legitimate coordination: ensuring only qualified practitioners serve the public. This perspective experiences genuine benefits from the credential requirement — reduced competition, maintained pricing power, professional prestige, and ability to set standards. The incumbents benefit from both the coordination function (genuine quality assurance) and the extraction function (artificial scarcity rent). From their position, the statute coordinates the profession AND secures their economic interests — these appear aligned.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY BOARD (TANGLED ROPE) — Occupies a genuinely hybrid position. The board coordinates legitimate public safety concerns (credential standards, disciplinary mechanisms, continuing education) and also captures rent for incumbents (controls supply, restricts entry, maintains pricing). Board members are often appointed from the incumbent practitioner pool, creating direct incentive alignment with restriction. The board's suppression of entrants is both justified (maintaining standards) and extractive (limiting competition). This is the perspective where the tangled nature is clearest.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL STATUTORY ARCHITECTURE (PITON) — At the macro institutional level, licensing statutes persist partly through genuine public safety justification but increasingly through theatrical maintenance of the rent mechanism. The theater shows through in: (a) credential inflation (degree requirements rising without corresponding risk increases), (b) continuing education requirements with low empirical tie to public safety, (c) reciprocity barriers between states despite identical actual competence, (d) disciplinary mechanisms that rarely remove practitioners for incompetence but strongly restrict entry. The formal apparatus persists because alternatives haven't fully replaced it and because institutional inertia is powerful, not because its primary function (public safety) would collapse without the current restrictive form.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From an analytical, civilizational, universal scope, the licensing statute is a pure extraction mechanism. Public safety concerns can be addressed through transparent competence testing, transparent disciplinary records, and liability frameworks without the artificial scarcity. The continued use of restrictive credentialing despite these alternatives reveals extraction as the primary function. The analytical observer sees clear beneficiaries (incumbents, boards), clear victims (entrants, consumers), suppression (legal barriers to entry), and minimal genuine coordination function that requires the current restrictive form.
constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(licensing_statute_mandate__rent_seeking_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, TR),
    TR >= 0.70.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial rents through artificial scarcity—consumers pay premium prices, entrants bear credential acquisition costs, and alternative credential pathways are legally blocked. The extraction is not maximal (0.70+) because some credential requirements genuinely serve quality assurance (though far less than currently required). Suppression (0.68): High. Entry barriers are legally enforced; entrants cannot practice without statutory credentials. Suppression mechanisms include: (1) legal prohibition (practicing without license = criminal/civil liability), (2) credential gatekeeping (only state-approved schools count), (3) reciprocity barriers (credentials from other states/countries rarely recognized), (4) fee extraction (licensing fees rise annually without corresponding value), (5) continuing education inflation (rising requirements disconnected from risk changes). Theater ratio (0.62): Moderate-high. The public safety justification is genuine but increasingly theatrical. Continuing education requirements show theater signatures: (a) weak empirical correlation with practitioner competence, (b) proliferation (every continuing education provider approved), (c) compliance over learning (checking boxes vs mastering material), (d) cost extraction (fees rise independent of content value). The theatrical component has grown as credential inflation has accelerated. Trajectory: The measurements show credential inflation (extractiveness rising from 0.35 to 0.58 over 50 years) and theater growth (from 0.42 to 0.62) despite no corresponding increase in occupational risk in most regulated fields. This pattern reveals extraction as the primary function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same statutory structure produces fundamentally different experienced constraint types depending on structural position. The incumbent sees this as solving a coordination problem (Rope—assuring consumer trust). The entrant sees this as insurmountable legal prohibition (Snare). The board sees genuine public safety goals mixed with rent extraction (Tangled Rope). The consumer sees price elevation with suppressed alternatives (Snare). The formal institutional architecture shows signs of piton (theatrical maintenance, declining primary function, persistence through inertia). The analytical observer, freed from institutional position, sees the extractive function clearly. The key insight: the snare classification from multiple victim perspectives (entrants, consumers, analytical observer) identifies which reading of the kernel is more structurally salient.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status plus exit options. Incumbents are beneficiaries with arbitrage exit (can move between regulated occupations, can leave regulation if profitable)—d ≈ 0.10, low extraction experienced. Entrants are victims with trapped exit (legally prohibited entry without credential)—d ≈ 0.92, maximum extraction experienced. Consumers are victims with constrained exit (can avoid service but at high practical cost)—d ≈ 0.75, high extraction. The regulatory board is simultaneously beneficiary (through regulatory capture) and victim (constrained by statutory obligation to enforce safety while boards extract rent)—d ≈ 0.50 with institutional power, producing the tangled rope classification. At the analytical level (observer position), d ≈ 0.78, revealing the constraint's extractive structure without institutional position distortion.
 *
 * MANDATROPHY ANALYSIS:
 *   RENT-SEEKING READING MANDATROPHY: This constraint resolves mandatrophy by distinguishing extraction from coordination through observable structural features: (1) Credential inflation independent of risk (extracts rent without safety benefit), (2) Board composition favoring incumbents (captures regulatory apparatus), (3) Suppression of alternatives despite comparable safety outcomes (protects rents over optimization), (4) Theater growth (continuing education disconnected from competence), (5) Interstate variation (identical services, different requirements—reveals political economy, not safety). If the statute were primarily coordinating public safety, we would expect: (a) minimal credential requirements (sufficient for safety, not more), (b) board diversity (representatives of consumers, practitioners, researchers), (c) transparent safety outcome metrics, (d) reciprocal recognition of equivalent credentials, (e) declining theater ratio as systems mature. The opposite pattern confirms rent-seeking as primary function masked by safety framing. The reading resolves the ambiguity by anchoring to observed behavior rather than stated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_necessity_threshold,
    'What portion of the current licensing restrictiveness is genuinely necessary for public safety versus purely extractive?',
    'Comparative analysis: jurisdictions with lower credential requirements (lower entry barriers, less restrictive continuing education); correlation between credential restrictiveness and actual disciplinary rates / consumer harm metrics. Interstate variation analysis: identical services with dramatically different licensing requirements across state lines.',
    'If necessary_portion > 70%: constraint should reclassify toward Tangled Rope. If necessary_portion < 30%: snare classification confirmed. If variation shows public safety maintained at lower restrictiveness levels: rent-seeking function becomes salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_necessity_threshold, empirical, 'Necessity threshold for public safety versus pure extraction').

omega_variable(
    disciplinary_mechanism_use,
    'Are licensing boards'' disciplinary mechanisms used primarily to maintain quality or to protect incumbents from low-cost competitors?',
    'Data on disciplinary actions: rate of removal for incompetence versus rate of restriction on alternative credentials; correlation between practitioner income and disciplinary case volume; analysis of discipline patterns for incumbents versus entrants attempting alternative pathways.',
    'If discipline heavily targets entrants/alternatives: supports snare classification and extraction function. If discipline primarily targets genuinely incompetent practitioners: supports genuine quality maintenance (Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_mechanism_use, empirical, 'Whether discipline prioritizes quality or protects incumbents').

omega_variable(
    credential_inflation_trajectory,
    'Are credential requirements increasing over time independent of corresponding increases in actual occupational risk or complexity?',
    'Historical analysis of credential requirements (1970-2026) versus job task complexity, consumer harm rates, and practitioner competence test scores. Identify fields where requirements have escalated (e.g., cosmetology) without corresponding risk changes.',
    'If inflation is independent of risk: credential requirements serve extraction rather than safety (Snare). If correlated with actual risk changes: Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_inflation_trajectory, empirical, 'Whether credential inflation correlates with actual occupational risk').

omega_variable(
    alternative_credential_efficacy,
    'Do alternative credentialing mechanisms (apprenticeship, direct competence testing, reciprocal recognition) produce equivalent public safety and consumer protection outcomes at lower cost?',
    'Comparative study of jurisdictions using alternative mechanisms (Switzerland apprenticeship model, occupations with direct testing pathways, military-to-civilian credential bridges); outcomes data on consumer safety, practitioner competence, service quality.',
    'If alternatives produce equivalent outcomes at lower cost: the restrictive statutory form is not necessary — extraction is the primary function (Snare confirmed). If alternatives produce worse outcomes: justifies Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_efficacy, empirical, 'Whether alternatives provide equivalent public safety at lower cost').

omega_variable(
    cross_reading_feasibility,
    'Can the same statutory kernel be genuinely read as public_safety_coordination (sibling reading) or only as rent-seeking suppression?',
    'Identify board compositions where incumbents do NOT hold seats; jurisdictions where entry requirements are low; cases where public safety standards are maintained despite low credential barriers. If such cases exist and produce equivalent safety outcomes, the public_safety_coordination reading is feasible. If such cases do not exist or produce inferior outcomes, the rent_seeking_suppression reading is more structural.',
    'Determines whether the readings coexist_with or forecloses. If coexists_with: different parties hold different readings legitimately. If forecloses: the structural data makes only the rent-seeking reading coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_feasibility, conceptual, 'Whether the statutory kernel can sustain multiple readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lic_rent_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lic_rent_tr_t25, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 25, 0.54).
narrative_ontology:measurement(lic_rent_tr_t50, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(lic_rent_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lic_rent_be_t25, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(lic_rent_be_t50, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lic_rent_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lic_rent_su_t25, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(lic_rent_su_t50, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, occupational_wage_dispersion).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, geographic_labor_mobility_barriers).

% DUAL FORMULATION NOTE:
% The statutory licensing mandate is ONE kernel with multiple structurally distinct readings. Each reading instantiates a different constraint with a different constraint_id. The 'rent_seeking_suppression' reading treats the statute as primarily extractive (Snare). The 'public_safety_coordination' reading treats it as primarily coordinative (Rope). The 'graduated_access_filter' reading treats it as hybrid (Tangled Rope). All three readings reference the same statutory text but decompose its structural function differently. Each story has its own ε, its own beneficiary/victim mapping, and its own classification. The network links reveal which reading's observables (credential inflation, theater growth, board capture) create structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
