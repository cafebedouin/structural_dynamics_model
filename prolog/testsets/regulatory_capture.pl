% ============================================================================
% CONSTRAINT STORY: regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture, []).

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
 *   constraint_id: regulatory_capture
 *   human_readable: Regulatory Capture
 *   domain: economic/political
 *
 * SUMMARY:
 *   Regulatory capture is a structural constraint where regulatory agencies
 *   created to protect the public interest become vehicles for incumbent-firm
 *   extraction. The constraint exhibits eight distinct perspectival readings:
 *   trapped consumers and market entrants experience pure extraction (Snare);
 *   incumbent firms experience it as coordination (Rope); industry
 *   associations and regulatory agencies experience it as mixed
 *   coordination-extraction (Tangled Rope); deregulation advocates see it as
 *   temporary with a sunset (Scaffold); the public interest narrative
 *   maintains performative commitment despite functional capture (Piton); and
 *   the analytical observer risks naturalizing capture as an immutable
 *   feature of regulatory systems (false Mountain). The extractiveness has
 *   increased over the 40-year measurement interval from 0.35 to 0.58,
 *   reflecting the progressive colonization of regulatory rule-making by
 *   industry actors. The theater ratio has risen from 0.42 to 0.64,
 *   indicating that regulatory justifications have become increasingly
 *   performative — standards are now written to appear neutral while
 *   operationally protecting incumbent rents.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — benefit from regulatory barriers that exclude competitors
 *   - Consumer Constituency: Primary victim (powerless/trapped) — cannot organize or exit; bears extraction via higher prices and reduced innovation
 *   - Market Entrants: Secondary victim (powerless/trapped) — face regulatory barriers to entry designed to protect incumbents
 *   - Regulatory Agency Leadership: Co-beneficiary (institutional/constrained) — career advancement dependent on post-government industry employment
 *   - Industry Association: Hybrid actor (organized/constrained) — coordinates industry standards (genuine rope function) while excluding outsiders (extraction)
 *   - Public Interest Advocates: Tertiary victim (moderate/constrained) — attempt to influence regulation but face asymmetric information and mobilization barriers
 *   - Deregulation Movement: Reformer (organized/mobile) — sees capture as temporary; building exit mechanisms through trade liberalization and jurisdictional competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture, 0.58).
domain_priors:suppression_score(regulatory_capture, 0.68).
domain_priors:theater_ratio(regulatory_capture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture, tangled_rope).
narrative_ontology:human_readable(regulatory_capture, "Regulatory Capture").
narrative_ontology:topic_domain(regulatory_capture, "economic/political").

domain_priors:requires_active_enforcement(regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture, incumbent_firms).
narrative_ontology:constraint_beneficiary(regulatory_capture, regulatory_agency_leadership).
narrative_ontology:constraint_victim(regulatory_capture, consumer_welfare).
narrative_ontology:constraint_victim(regulatory_capture, market_entrants).
narrative_ontology:constraint_victim(regulatory_capture, public_interest).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER CONSTITUENCY (SNARE) — Consumers cannot exit the market or organize collective action against regulatory distortion. They experience only the extraction mechanisms: higher prices, reduced product quality, and blocked innovation. No representation in the regulatory process. Maximum extractiveness from the perspective of the trapped, unorganized beneficiary-in-name-only.
constraint_indexing:constraint_classification(regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARKET ENTRANT (SNARE) — New firms face regulatory barriers designed to protect incumbents. Even when they have superior products or lower costs, compliance costs and strategic regulatory enforcement trap them. No exit except acceptance of inferior market position or relocation to unregulated jurisdictions. Full victim status — extraction runs entirely from this agent.
constraint_indexing:constraint_classification(regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Incumbent firms experience the regulatory apparatus as a coordination mechanism that solves the collective action problem of preventing destructive competition. Regulatory barriers prevent underpricing and quality wars that would erode profit margins. Firms can arbitrage between jurisdictions with different regulatory intensity. Net beneficiary — experiences the constraint as pure coordination, not extraction.
constraint_indexing:constraint_classification(regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRY ASSOCIATION (TANGLED ROPE) — Trade associations coordinate information standards and compliance frameworks among competing firms. This is genuine coordination (rope function). However, the association also enforces exclusionary rules against outsider firms and lobbies for regulatory barriers. The constraint exhibits both coordination (among members) and extraction (against non-members). Active enforcement required. Organized power with constrained exit — can lobby but cannot unilaterally control regulatory outcomes.
constraint_indexing:constraint_classification(regulatory_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — The agency has genuine coordination functions (setting standards, resolving disputes, enforcing baseline safety rules). But the same agency becomes captured when industry actors dominate the rule-writing process through revolving-door hiring, lobbying, and information asymmetry. The agency experiences extraction pressure: its leadership's career path depends on post-government industry employment. Exit is constrained by the political economy of budget allocation and leadership recruitment. Institutional actor with constrained exit — experiences both coordination and extraction in high proportions.
constraint_indexing:constraint_classification(regulatory_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC INTEREST NARRATIVE (PITON) — The regulatory system maintains a performative commitment to 'public interest' governance. Regulatory documents invoke consumer protection, safety, and fair competition. However, this narrative has become largely theatrical — the functional mechanism is now capture. The public interest language persists because the system's legitimacy depends on it, not because the apparatus serves it. Theater ratio 0.64 reflects the gap between stated mission and actual function. The constraint persists through institutional inertia, not because the public interest framing is accurate.
constraint_indexing:constraint_classification(regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DEREGULATION MOVEMENT (SCAFFOLD) — From the perspective of organized reformers (libertarian think tanks, startup advocacy groups, international trade bodies), regulatory capture is a temporary institutional arrangement with a sunset clause. Deregulation, trade liberalization, and trans-jurisdictional competition create exit paths around captured regulators. This perspective sees the constraint as a degraded scaffolding that will collapse as market forces and global mobility increase. The sunset is real — globalization and digital disruption are reducing regulator control — but it is slow and unevenly distributed across sectors.
constraint_indexing:constraint_classification(regulatory_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ECONOMIC THEORY VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, regulatory capture is an unavoidable consequence of information asymmetry and concentrated interests. The regulator cannot have perfect information; industry experts know their domain better than public-interest advocates. Interest groups with specific stakes in regulation will always have stronger mobilization than diffuse consumer constituencies. This perspective sees capture as an immutable feature of regulatory systems, analogous to a physical constant. However, this naturalization obscures that capture intensity varies dramatically by institutional design (parliamentary vs executive, transparency rules, conflict-of-interest restrictions, term limits). The mountain classification is a false summit — it mistakes a prevalent institutional outcome for an immutable law.
constraint_indexing:constraint_classification(regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Regulatory capture operates through several extraction mechanisms: (1) price protection — incumbent firms face reduced competition and can maintain supra-competitive pricing; (2) quality suppression — barriers to innovation prevent entrants with superior products from displacing incumbents; (3) information rent — regulatory compliance costs are deliberately set above what actual safety/quality protection requires, extracting excess surplus; (4) career asymmetry — regulators extract a continuation option (post-government industry employment) not available to other government employees. The trajectory from 0.35 to 0.58 reflects progressive tightening of the capture mechanism. Suppression (0.68): High. Multiple barriers prevent exit: (a) legal barriers — formal regulatory requirements exclude entrants; (b) information barriers — compliance complexity; (c) political barriers — captured regulator blocks rule changes; (d) career barriers — potential reformers know industry jobs depend on not antagonizing future employers. Theater ratio (0.64): Moderate-high. Regulatory documents and press releases invoke consumer protection and fair competition, but functional outcome is incumbent rent protection. The gap reflects Goodhart drift — regulatory metrics (e.g., 'safety standards met') have decoupled from their intended purpose (actual consumer safety improvement).
 *
 * PERSPECTIVAL GAP:
 *   Regulatory capture produces a maximal perspectival gap: eight distinct readings of the same structural phenomenon. The incumbent firm sees rope (coordination benefit). The consumer sees snare (pure extraction). The regulator sees tangled_rope (mixed coordination-extraction with career pressure). The reformer sees scaffold (temporary, solvable by deregulation). The public interest frame sees piton (performative commitment to a mission that no longer functions). The analytical observer risks seeing mountain (immutable law of regulatory systems). This gap is not an artifact of measurement — it reflects genuine differences in structural position, exit options, and benefit/cost asymmetry. The framework's value is to make these differences explicit rather than collapsing them into a single type. Mandatrophy resolution: The constraint avoids mandatrophy by maintaining clear structural distinctions between the coordination function (industry standard-setting, technical dispute resolution — genuinely serving all market participants) and the extraction function (barrier maintenance, price support, innovation suppression — serving only incumbents). The tangled_rope classification correctly identifies both functions as present and active. A pure-rope classification would be false (ignoring extraction); a pure-snare classification would be false (ignoring genuine coordination services). The mixed classification accurately reflects that capture has both real coordination content and real extraction content, and the balance has shifted toward extraction over the 40-year interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory capture derives directionality from the asymmetry between concentrated beneficiaries and diffuse victims. Incumbent firms occupy a position of high benefit (they extract regulatory protection) combined with high exit optionality (they can lobby, relocate, or adjust compliance strategy). This produces low d → negative χ from their perspective. Consumers occupy a position of high cost (they pay supra-competitive prices) combined with zero exit optionality (they cannot opt out of a regulated market or organize collectively). This produces high d → high χ from their perspective. The regulatory agency occupies an intermediate position: it provides genuine coordination services (standard-setting, dispute resolution) but has constrained exit due to career dependence on industry hiring. This produces moderate d. The directionality derivation also captures the paradox of the industry association: it is both a beneficiary (it excludes competitors) and subject to enforcement pressure (it must comply with regulatory rules). This mixed position is reflected in its tangled_rope classification — beneficiary status + victim status + constrained exit → moderate d with both coordination and extraction present.
 *
 * MANDATROPHY ANALYSIS:
 *   Regulatory capture is not classified as snare despite high extractiveness (0.58) because the constraint has genuine coordination content — industry associations coordinate technical standards, the regulatory agency resolves genuine disputes, and information standardization has real public benefits. A snare classification would require extractiveness ≥ 0.46, suppression ≥ 0.60, and χ ≥ 0.66 (all met) BUT also minimal coordination benefit — which is false here. The constraint fails the snare gate because the beneficiaries (incumbent firms) genuinely benefit from coordination, not just extraction. The tangled_rope classification correctly identifies that the constraint is a hybrid: it solves real coordination problems (technical standards, baseline safety rules) while simultaneously extracting through exclusion and rent protection. The theater ratio (0.64) reflects Goodhart drift — regulatory language persists (public interest, consumer protection) even as the functional mechanism has shifted toward incumbent service. If theater_ratio exceeded 0.70 and extractiveness fell below 0.46, the constraint would reclassify as piton (degraded former rope, now mostly theatrical). The current classification (tangled_rope) indicates that capture is still functional — it actively serves incumbents through real barrier maintenance, not just symbolic compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_intensity_threshold,
    'At what point does regulatory standard-setting become capture rather than legitimate industry consultation?',
    'Empirical analysis of regulatory outcome distributions: compare jurisdictions with different consultation rules; measure correlation between industry lobbying expenditure and regulatory outcomes; survey regulator decision-making process transparency',
    'If threshold is low (industry input alone sufficient): most regulation is capture. If threshold is high (requires demonstrable consumer benefit): capture is narrower but harder to detect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_intensity_threshold, empirical, 'Threshold distinguishing legitimate industry consultation from regulatory capture').

omega_variable(
    public_interest_coalitional_capacity,
    'Can dispersed consumer constituencies overcome collective action problems to counter-organize against incumbent capture, or is the asymmetry intrinsic to regulatory democracy?',
    'Historical case studies of successful consumer mobilization against regulatory capture (e.g., pharmaceutical pricing reform, telecommunications deregulation); measurement of grassroots vs industry lobbying expenditure ratios and their correlation with regulatory outcomes',
    'If coalitional capacity exists: capture is contingent and can be reversed. If asymmetry is intrinsic: capture will persist absent institutional redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_interest_coalitional_capacity, empirical, 'Whether consumer constituencies can overcome collective action barriers').

omega_variable(
    deregulation_sustainability,
    'Is deregulation-via-competition a permanent exit mechanism from capture, or does it create new regulatory demand that resurrects capture in different forms?',
    'Long-term case study of industries that underwent deregulation and track the formation of new regulatory structures, private certification schemes, or industry self-regulation that replicate capture dynamics',
    'If sustainable exit: the scaffold perspective is correct and capture will fade with market expansion. If re-capture occurs: deregulation is temporary and the constraint cycle repeats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deregulation_sustainability, empirical, 'Whether deregulation provides durable exit from regulatory capture').

omega_variable(
    international_regulatory_arbitrage,
    'Do multinational firms and jurisdictional shopping permanently weaken any single regulator''s capture power, or do harmonized international standards create super-national capture?',
    'Analysis of regulatory harmonization trends (ISO, trade agreements); measurement of firm compliance costs under multi-jurisdictional regulation; case studies of international standard-setting bodies (e.g., Basel Committee) for signs of capture at the super-national level',
    'If arbitrage weakens single-regulator capture: global mobility is an effective constraint on local capture. If harmonization creates super-national capture: escape to another jurisdiction is no longer possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_arbitrage, empirical, 'Whether international regulatory arbitrage undermines local capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture, theater_ratio, 20, 0.54).
narrative_ontology:measurement(regcap_tr_t40, regulatory_capture, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(regcap_be_t40, regulatory_capture, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture, market_entry_barriers).
narrative_ontology:affects_constraint(regulatory_capture, information_asymmetry_in_compliance).
narrative_ontology:affects_constraint(regulatory_capture, revolving_door_employment).

% DUAL FORMULATION NOTE:
% Regulatory capture is upstream of specific sector-level barriers (pharmaceutical approval, telecommunications licensing, financial services regulation). Each sector has its own constraint story reflecting sector-specific extractiveness values, but all are downstream of the general capture mechanism. The network links represent structural dependence: sector-level barriers persist because the general capture mechanism prevents regulatory reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
