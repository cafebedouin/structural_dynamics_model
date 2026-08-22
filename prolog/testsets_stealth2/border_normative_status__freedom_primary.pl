% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Exclusion Regime (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This story instantiates the freedom_primary reading of the
 *   border_normative_status kernel: freedom of movement is a fundamental
 *   human right, borders impermissibly restrict it, and exclusion requires
 *   extraordinary justification. Per the epsilon-referent rule for
 *   kernel-reading stories, epsilon's referent is the standing arrangement
 *   under contest — the global border-exclusion regime as it actually
 *   operates — assessed by this reading's own lights, which yields a high
 *   value: on this reading nearly all ordinary exclusion fails any
 *   extraordinary-justification bar, so the arrangement operates as a
 *   sustained rights violation rather than a neutral administrative filter.
 *   The sibling readings are separate constraints, not positions inside this
 *   one: sovereignty_primary (territorial exclusion as foundational
 *   collective self-determination) authors a near-zero epsilon over the same
 *   referent because it finds little wrongfully taken; qualified_sovereignty
 *   authors an intermediate value, wronging only disproportionate or
 *   rights-inconsistent exclusion. Same referent, reading-indexed values. The
 *   three stories form a constraint family linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   destination_state_governments — agenda-setting institutional actor,
 *   writes and enforces the rules and collects the fiscal and electoral
 *   returns of closure; destination_country_citizens — primary beneficiary
 *   seat (organized, mobile), holders of closed membership pools who finance
 *   the enforcement that maintains them; employers_of_deportable_labor —
 *   concentrated beneficiary, collecting compliance leverage over a workforce
 *   that cannot safely refuse or organize; border_enforcement_industry —
 *   secondary beneficiary whose revenue scales with enforcement intensity;
 *   excluded_migrants — primary target (powerless, trapped), bearing denial
 *   of entry, lethal irregular routes, and family separation;
 *   undocumented_residents — target under standing enforcement exposure
 *   inside the territory; asylum_seekers_externalized — target of legal
 *   channels relocated out of reach; exposed_sector_domestic_workers —
 *   second-ring target whose pay and conditions are pressed down by
 *   substitutable deportable labor (this reading's structural delta places
 *   them in the wronged set); transit_state_governments — inter-institutional
 *   seat paid to host the burden they did not design;
 *   origin_country_communities — excluded voice, no seat in any forum that
 *   decides their members' movement; human_rights_institutions — analytical
 *   observer with findings but no enforcement capacity.
 *
 * KEY AGENTS:
 *   - destination_state_governments: agenda_setter (institutional/arbitrage) — writes visa schedules, funds enforcement, signs externalization agreements; collects closure's fiscal and electoral returns
 *   - destination_country_citizens: primary beneficiary (organized/mobile) — hold closed wage, welfare, and residence pools; finance the enforcement through taxation
 *   - employers_of_deportable_labor: concentrated beneficiary (powerful/arbitrage) — collect compliance leverage and suppressed wage floors in exposed sectors
 *   - border_enforcement_industry: secondary beneficiary (institutional/mobile) — revenue scales with enforcement intensity
 *   - excluded_migrants: primary target (powerless/trapped) — bear refusal of lawful entry, irregular-route mortality, separation
 *   - undocumented_residents: target (powerless/trapped) — live under standing removal exposure; deportability disciplines their wages and organizing
 *   - asylum_seekers_externalized: target (powerless/trapped) — legal channel relocated offshore, ahead, or out of reach
 *   - exposed_sector_domestic_workers: second-ring target (moderate/constrained) — bear sector-wide pay and condition pressure from substitutable deportable labor
 *   - transit_state_governments: inter-institutional payer/beneficiary (institutional/constrained) — accept payment to patrol and host, bargain from aid dependence
 *   - origin_country_communities: excluded voice (powerless) — their members' movement decided entirely in forums where they hold no seat
 *   - human_rights_institutions: analytical observer (institutional/analytical) — issue findings that ordinary exclusions lack adequate justification; no enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.8).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.82).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.8).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Exclusion Regime (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '6c87508a-893a-4c53-b9c4-926ed68258b5').
narrative_ontology:cs_kernel_codification('6c87508a-893a-4c53-b9c4-926ed68258b5', distributed).
narrative_ontology:cs_authority_grounding('6c87508a-893a-4c53-b9c4-926ed68258b5', lineage).
narrative_ontology:cs_interpretation_layer_present('6c87508a-893a-4c53-b9c4-926ed68258b5').
narrative_ontology:cs_reading_relation('6c87508a-893a-4c53-b9c4-926ed68258b5', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('6c87508a-893a-4c53-b9c4-926ed68258b5', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('6c87508a-893a-4c53-b9c4-926ed68258b5', foundational, movement_is_presumptive_human_right).
narrative_ontology:cs_axiom_status(movement_is_presumptive_human_right, holdable).
narrative_ontology:cs_axiom_grounding('6c87508a-893a-4c53-b9c4-926ed68258b5', movement_is_presumptive_human_right, deontological).
narrative_ontology:cs_axiom('6c87508a-893a-4c53-b9c4-926ed68258b5', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('6c87508a-893a-4c53-b9c4-926ed68258b5', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('6c87508a-893a-4c53-b9c4-926ed68258b5', movement_as_presumptive_liberty).
narrative_ontology:cs_drift_state('6c87508a-893a-4c53-b9c4-926ed68258b5', contemporary_externalization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6c87508a-893a-4c53-b9c4-926ed68258b5', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_country_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_industry).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, undocumented_residents).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers_externalized).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, exposed_sector_domestic_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_state_governments).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, transit_state_governments).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, destination_country_citizens).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, transit_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and interior ministries write the visa schedules, fund and direct the enforcement agencies, sign externalization and readmission agreements, and decide case-by-case which entries to permit. They collect the fiscal and electoral returns of membership closure and can redesign, tighten, or suspend the rules at will; their principal brake is the domestic coalition that prefers the line held rather than moved.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, destination_state_governments, beneficiary).

% Hold unconditional membership in the protected territories: residence, work, and movement among themselves without the paperwork everyone else faces, plus wage and service pools closed to outsiders. They finance the enforcement apparatus through taxation and staff its politics with their votes; a minority of them compete for work in the sectors where a deportable labor supply is present.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_country_citizens, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, destination_country_citizens, payer).

% In agriculture, construction, care, and hospitality they hire from a workforce whose right to stay depends on continued employment. Wage floors sit lower, collective action carries removal risk for the workers who attempt it, and conditions deteriorate without the staff leaving. They lobby for enforcement calibrated to keep supply insecure enough to be compliant and steady enough to be usable.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, employers_of_deportable_labor, beneficiary,
    powerful, biographical, arbitrage, national).

% Suppliers of surveillance platforms, biometric databases, detection technology, detention and removal services, patrol vessels, and consultancy. Revenue scales with enforcement intensity, so the industry commissions threat assessments and markets capacity expansion to the governments that appropriate the budgets.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_industry, beneficiary,
    institutional, biographical, mobile, continental).

% People who want to cross for work, safety, or family and are refused lawful entry or residence. Their realistic options are remaining in the conditions they sought to leave, paying smugglers for irregular routes that kill thousands annually, or waiting years for legal channels thinner than the demand. Family separation, foregone lifetime earnings, and detention are borne here.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Live and work inside the territory without authorization, under standing exposure to arrest and removal. Deportability keeps pay low, hours long, and organizing dangerous; reporting a crime or a workplace injury invites the enforcement they most fear, so abuse goes unreported.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, undocumented_residents, payer,
    powerless, immediate, trapped, national).

% Flee persecution and find the legal channel relocated out of reach: offshore processing, safe-third-country designations, pushbacks at the frontier, and visa rules that make arrival by air impossible. They wait in transit states, often for years, in conditions the arrangements were designed to produce.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers_externalized, payer,
    powerless, immediate, trapped, continental).

% Citizen and regular-status workers in trades where employers can substitute a deportable workforce. Downward pressure on pay and standards follows the sector rather than the firm, so switching jobs does not escape it. They wrote none of the rules and receive little of what the rules protect; their grievance is with the design, not with the workers they are set against.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, exposed_sector_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Governments along the migration corridors accept externalization funding, equipment, and diplomatic support in exchange for patrolling routes and hosting people who would otherwise arrive elsewhere. They carry the humanitarian and fiscal burden of stranded populations while their bargaining position depends on continued aid, which limits how hard they can push back on the terms.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, transit_state_governments, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, transit_state_governments, beneficiary).

% Households and villages whose members' ability to leave, return, or circulate is decided entirely by destination-state rules in forums where they hold no seat. They absorb the costs of prolonged separation and broken circular migration, and their household and local development strategies adapt around whatever channels happen to remain open.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, origin_country_communities, excluded,
    powerless, generational, constrained, global).

% Treaty bodies, special rapporteurs, and regional courts review state practice against movement-rights commitments, issue findings that the great majority of ordinary exclusions lack adequate justification, and command attention, precedent, and reputational force — but no enforcement capacity of their own. States comply selectively and cite reservations where the findings bite.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, employers_of_deportable_labor).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Border administration solves real coordination problems: verifying identity at entry, screening for health and security risks, collecting customs, and maintaining the membership registers on which taxation, welfare eligibility, and political representation depend. This story's scope is the exclusionary-restriction function layered on top of that machinery — the denial of entry and residence itself — which is the object the freedom_primary reading contests; the screening-and-documentation layer is separable and would warrant its own story.
% TRANSFER_FUNCTION: Moves access — to territory, labor markets, safety, and family life — from non-members to member-citizens and employers; moves enforcement procurement from taxpayers to the enforcement industry; and converts a portion of the would-be workforce into a deportable labor supply whose suppressed wages and disciplined organizing flow upward to the employers who hire it.
% ABSENT_VOICES: Excluded migrants and would-be movers are the paradigmatic absent voices: subject to rules made in legislatures where they hold no vote, in negotiations where they hold no chair. Origin-country communities are absent a second time over — their members' movement is decided entirely elsewhere. Future generations in low-income countries inherit lines drawn without their input. The unanimity with which destination-state politics treats closure as settled owes much to these seats never being in the room.
% DISAPPEARANCE_RATIONALE: If the exclusion regime vanished overnight, labor markets would reorganize within seasons as workers moved toward productivity gaps, remittance economies would swell and then normalize into ordinary circular migration, the enforcement industry would lose its revenue base, demographic trajectories of aging destination societies would bend, and citizenship would shed its current meaning as a mobility monopoly. Nothing about the surrounding world depends on the arrangement continuing in its present form; much depends on it not changing abruptly.
% FOUNDING_PROBLEM: The arrangement was built to solve sovereign membership management: screening entrants for security and disease, protecting domestic wage and welfare pools from outside claimants, and maintaining collective self-determination over who joins the polity.
% FOUNDING_PROBLEM_CORROBORATION: Destination-state governments attest the founding problems remain live, citing security threats and welfare sustainability. From outside the benefiting parties: UN Human Rights Committee General Comment No. 27 and successive treaty-body reviews attest that the overwhelming majority of exclusions on offer fail any proportionate justification test; the development-economics literature estimating that labor-mobility barriers destroy output on a scale exceeding most other policy distortions attests that the restriction's cost dwarfs its stated protective aims; and migrant-led organizations attest from lived position that the arrangement's operative function is exclusion itself rather than screened admission. No source independent of the benefiting parties attests that ordinary, interest-based exclusion satisfies an extraordinary-justification bar — the absence of such corroboration is itself signal.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.80 at interval end) because the referent — the standing exclusion arrangement — is assessed by this reading's lights: the overwhelming majority of current exclusions rest on ordinary interests (labor-market protection, welfare-pool closure, electoral preference) that fail an extraordinary-justification bar, and the costs borne by the excluded (foregone lifetime earnings on the order of the largest known policy-induced losses in economics, family separation, irregular-route mortality) dwarf the arrangement's stated protective aims. Suppression is authored 0.82 as a raw structural property — walls, patrols, biometric registries, carrier sanctions, externalized processing that closes the asylum channel before arrival — and per the framework it is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater ratio 0.48: a substantial and growing share of enforcement activity is symbolic (headline deportation counts, wall construction, port security choreography) rather than functional screening, though genuine screening and documentation work continues. Accessibility collapse is moderate-low (0.42): alternatives are understood and demonstrated — free-movement blocs prove a different design operates at scale, remote work erodes the territory-wage link, irregular routes persist — so the arrangement prices and punishes alternatives rather than collapsing them. Resistance 0.58: litigation, advocacy, abolitionist scholarship, route struggles, and labor-solidarity organizing meet the arrangement continuously without systemic overturn; the classic coalition path for powerless targets (excluded migrants allying with exposed-sector domestic workers) is the live vector and partly powers the resistance figure. The measurement series run on one shared eight-point grid (every tracked metric authored at every point, 1950–2020 mapping): extractiveness rises as visa regimes globalized and welfare closure deepened; theater rises with symbolic enforcement politics; suppression_requirement traces a deliberate enforcement ratchet — the machinery (biometrics, joint patrols, externalization deals) visibly built up over the interval, which is why suppression_requirement is tracked at all rather than left to the static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the citizen and employer seats the arrangement presents as membership maintenance and workforce management — low personal burden, high personal yield, so a coordination-flavored verdict is available from those chairs. From the excluded, undocumented, and externalized seats the identical structure presents as a standing rights violation administered against them personally — full-target directionality with trapped exit produces maximal effective burden. Transit-state governments experience both faces at once: paid to enforce a line whose stranded populations they host. Human-rights institutions see the whole structure and can name it but cannot move it. Nothing in the authored claim adjudicates between these experiences; the structural data (roles, power, exit, scope) drive the per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Beneficiaries sit near the subsidized end: destination_country_citizens (organized, mobile exit — they hold the very freedom of movement the arrangement denies others, an arbitrage-grade asymmetry), employers_of_deportable_labor (arbitrage exit, concentrated gains), border_enforcement_industry (mobile, contract-driven gains). Targets sit near the full-target end: excluded_migrants, undocumented_residents, and asylum_seekers_externalized are powerless with trapped exit and identity-relevant stakes, the configuration that maximizes effective burden; scope amplifies further since the arrangement operates globally and nationally where verification is hard and exit hardest. Exposed_sector_domestic_workers derive a partial-target value (constrained exit, moderate power, sector-bound costs). Transit_state_governments derive a near-symmetric value from their dual payer/beneficiary position. Origin_country_communities, though role-excluded rather than role-payer, bear real costs with no seat; the derivation reads their powerlessness and global scope as substantial target-side weight. Human_rights_institutions sit analytical and near-symmetric. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the relationships described, and overriding without structural need would launder the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved here and should not be: the arrangement's functions are actively performed, its enforcement machinery is expanding rather than atrophying, and nothing about it is maintained by inertia alone. The mislabeling risk runs in the opposite direction from the usual zombie case — the danger is rope-washing, in which the arrangement's genuine coordination residue (identity verification, health and security screening, membership registers that taxation and representation depend on) launders the exclusionary core as mere administration. The epsilon-invariance discipline handles this cleanly: the screening-and-documentation function is a separable constraint that could be authored as its own story (where it would likely compute as a low-extraction coordination mechanism), and this story's scope is deliberately the exclusionary-restriction function itself, which is what the freedom_primary reading is about. Keeping founding_problem_status contested rather than dead preserves the genealogy honestly: screening problems are live, but whether they justify ordinary exclusion is precisely what the readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_freedom_primary,
    'This constraint is one reading (freedom_primary) of the border_normative_status kernel. Which reading correctly locates the justificatory burden for territorial exclusion, and therefore who counts as wronged?',
    'Dialectical and jurisprudential tracking: observe which reading''s premises treaty-body jurisprudence, state practice, and philosophical argument converge on; the sibling stories (sovereignty_primary, qualified_sovereignty) carry the same referent with reading-indexed epsilon values, so cross-reading comparison of computed verdicts against settled doctrine resolves the location.',
    'Under sovereignty_primary, excluded migrants leave the wronged set entirely (exclusion is a legitimate instrument of collective self-determination) and epsilon for the same standing arrangement drops toward negligible. Under qualified_sovereignty, only disproportionate or rights-inconsistent exclusion wrongs, splitting the wronged set. Under this reading, nearly all ordinary exclusion wrongs, and enforcement itself becomes the rights violation requiring justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_freedom_primary, conceptual, 'Committer-frame omega: which reading of the border_normative_status kernel governs, and how the victim set and epsilon shift across readings.').

omega_variable(
    extraordinary_justification_threshold,
    'What actually counts as an extraordinary justification for exclusion under this reading — individualized criminal or epidemiological risk only, or also macro-level public-order and fiscal-sustainability claims?',
    'Case-level adjudication: apply the bar to the actual inventory of exclusion practices (visa refusals, quota systems, externalized processing, pushbacks) and count what survives; treaty-body proportionality analysis supplies the working method.',
    'A narrow threshold leaves almost no current exclusion standing, confirming the arrangement as rights-violating through and through; a broad threshold preserves a coordination residue and shifts the computed type toward a hybrid with a genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, conceptual, 'Where the extraordinary-justification bar sits determines how much of the standing arrangement survives scrutiny.').

omega_variable(
    displaced_worker_cost_attribution,
    'Are the losses of exposed-sector domestic workers attributable to the border regime''s deportable-labor mechanism itself, or to trade, technology, and sectoral change that would operate regardless?',
    'Comparative sectoral analysis: compare wage and condition trajectories in sectors with high versus low exposure to a deportable workforce, controlling for automation and trade shocks; natural experiments from legalization programs that removed deportability leverage.',
    'If attribution to the regime holds, exposed-sector workers belong in the wronged set (per this reading''s structural delta) and the arrangement''s cost structure is broader than the excluded alone; if not, they exit the set and the wronged population narrows to those directly denied movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_cost_attribution, empirical, 'Whether the second-ring wronged set (domestic workers in exposed sectors) is a real cost of the arrangement or an artifact of confounding.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the observed immobility of would-be movers reflects structural barriers (visas, patrols, carrier sanctions) versus internalized deterrence (learned futility, ambient fear, self-limitation that persists where legal channels technically exist)?',
    'Post-liberalization trajectory: track mobility uptake where channels suddenly open (visa waivers, free-movement accession); if movement surges immediately, suppression was structural; if uptake lags despite open channels, an internalized component persists.',
    'If internalized deterrence is substantial, the arrangement''s effective hold on behavior exceeds its visible enforcement machinery, and measured suppression understates the true restraint; the structural measure reported here is the floor, not the ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression holding would-be movers in place.').

omega_variable(
    deadweight_vs_transfer_share,
    'How much of what the arrangement removes from the excluded is destroyed outright (foregone output never produced, lives never lived where chosen) versus transferred to identifiable recipients (compliance rents, enforcement contracts)?',
    'Decompose the aggregate: estimate foregone-migrant-output losses separately from measurable transfers (wage suppression differentials, enforcement procurement flows, remittance-fee skimming).',
    'A high deadweight share means the named receipt seat captures only a slice of the total and the arrangement destroys more than it moves; a high transfer share strengthens the concentration reading and the political-economy account of why the arrangement holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_vs_transfer_share, empirical, 'Split of the arrangement''s total cost to the excluded between destroyed value and value captured by named seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__freedom_primary, theater_ratio, 10, 0.28).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__freedom_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.33).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.36).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__freedom_primary, theater_ratio, 50, 0.4).
narrative_ontology:measurement(bord_tr_t60, border_normative_status__freedom_primary, theater_ratio, 60, 0.44).
narrative_ontology:measurement(bord_tr_t70, border_normative_status__freedom_primary, theater_ratio, 70, 0.48).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t10, border_normative_status__freedom_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bord_be_t20, border_normative_status__freedom_primary, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(bord_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(bord_be_t50, border_normative_status__freedom_primary, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(bord_be_t60, border_normative_status__freedom_primary, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(bord_be_t70, border_normative_status__freedom_primary, base_extractiveness, 70, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bord_su_t10, border_normative_status__freedom_primary, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(bord_su_t20, border_normative_status__freedom_primary, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(bord_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(bord_su_t50, border_normative_status__freedom_primary, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(bord_su_t60, border_normative_status__freedom_primary, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(bord_su_t70, border_normative_status__freedom_primary, suppression_requirement, 70, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the ethics of borders' decomposes into three structurally distinct constraints — one per reading of the border_normative_status kernel — because the readings assign different justificatory thresholds and therefore different victim sets and different epsilon values over the same standing arrangement. Upstream/downstream structure: sovereignty_primary is the historically dominant reading and is cited as the warrant for the enforcement architecture this story assesses; freedom_primary and qualified_sovereignty both press human-rights-lineage arguments against it, with freedom_primary taking the stronger premise. Additionally, within this reading's own scope a further decomposition is available: the screening-and-documentation function of border administration is separable from the exclusionary-restriction function and would warrant its own story (likely computing as low-extraction coordination); this story intentionally scopes to the restriction function, which is the object the freedom_primary reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
