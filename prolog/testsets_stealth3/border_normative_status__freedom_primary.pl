% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Freedom-Primary Reading: Border Exclusion Regime as Impermissible Restriction
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   A kernel-reading story. The standing arrangement under contest is the
 *   contemporary international border-control regime: universal visa
 *   architecture, interdiction, detention, deportation, and externalized
 *   enforcement, operated by destination states and a surrounding industry.
 *   This file authors that arrangement as assessed by the freedom_primary
 *   reading — movement as a fundamental human right that borders
 *   impermissibly restrict, exclusion requiring extraordinary justification.
 *   Epsilon's referent is the standing arrangement (never this reading's
 *   endorsed open-movement alternative); values are reading-indexed over the
 *   fixed referent. The reading's expected structural delta is realized in
 *   the data: excluded migrants anchor the victim set with no
 *   legitimate-exclusion carve-out, displaced domestic workers join the
 *   victim set through the regime's captive-labor channel, and enforcement
 *   appears as a rights violation carrying the burden of justification.
 *   Sibling readings (sovereignty_primary, qualified_sovereignty) are
 *   separate stories linked by network edges; this file does not hedge or
 *   average epsilon across them. Claim and metrics are independent authored
 *   facts: snare is the type I believe structurally true from this seat, and
 *   the metric values are what I believe descriptively true; neither was
 *   tuned toward the other or toward any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - excluded_migrants: Primary target (powerless/trapped, global) — bears the regime's full coercive weight; the group whose movement the constraint exists to deny
 *   - undocumented_resident_workers: Secondary target (powerless/trapped, national) — the deportable labor caste the regime manufactures inside destination states
 *   - displaced_domestic_workers: Collateral target (moderate/constrained, national) — citizens undercut by the captive-labor channel the regime produces
 *   - destination_state_governments: Agenda setter (institutional/mobile, national) — administers enforcement, collects political capital, could dissolve the apparatus by statute
 *   - employers_of_deportable_labor: Concentrated beneficiary (powerful/arbitrage, national) — draws the recurring wage differential the regime secures
 *   - border_security_contractors: Beneficiary (organized/arbitrage, global) — procurement revenue scaling with enforcement intensity
 *   - affluent_state_citizenries: Diffuse beneficiary (powerful/generational, national) — receives wage premia, welfare closure, bounded-membership identity; the electoral authorizer
 *   - human_smuggling_networks: Parasitic beneficiary (organized/immediate, continental) — monetizes the barrier itself; vanishes if the constraint falls
 *   - migrant_rights_movements: Resisting observer (organized/constrained, global) — legal aid, rescue, sanctuary, documentation; pays in repression
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical, global) — monitors, comments, litigates; holds no enforcement force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.85).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.88).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Freedom-Primary Reading: Border Exclusion Regime as Impermissible Restriction").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51').
narrative_ontology:cs_kernel_codification('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', formalized).
narrative_ontology:cs_authority_grounding('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', distributed).
narrative_ontology:cs_reading_relation('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', foundational, movement_is_fundamental_human_right).
narrative_ontology:cs_axiom_status(movement_is_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', movement_is_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', unrestricted_movement_default).
narrative_ontology:cs_drift_state('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', contemporary_hard_border_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2f21c6ef-e1cf-4b6c-a6d1-a9de79bd1f51', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_state_governments).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_security_contractors).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, affluent_state_citizenries).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, human_smuggling_networks).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, undocumented_resident_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, migrant_rights_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People seeking to cross international borders for work, asylum, or family reunification. They bear the regime's full coercive weight: interdiction at sea and land, detention, deportation, and death along peripheral routes. Exit is unavailable in principle — every habitable territory enforces the same structure, so fleeing one border regime means entering another, and the movement that would constitute exit is precisely what the regime denies. They hold no vote in any jurisdiction that sets the terms they live under.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Live and work inside destination states without legal status, often for decades. Deportability disciplines their wages, hours, and tolerance of abuse; reporting violations invites removal. The regime manufactures this captive labor pool and depends on it: enforcement intensity is calibrated to keep workers present but perpetually removable. Leaving means abandoning accumulated livelihoods; staying means indefinite exposure.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Citizens and legal residents employed in sectors where employers substitute deportable labor: construction, agriculture, food processing, care work, hospitality. They receive the regime's promised protection in theory, but the regime's actual product — a hyper-vulnerable labor caste — undercuts their wages and conditions. Retraining or relocating out of affected sectors and regions is costly and only partially available.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Set visa categories, enforcement budgets, detention policy, and externalization agreements; administer the apparatus and answer electorally for it. Collect political capital from enforcement posture plus fees, fines, and sponsored deterrence deals; bear enforcement expenditure and diplomatic friction. They could dissolve the apparatus by statute at any time; their exit option is policy mobility, not geography.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_state_governments, agenda_setter,
    institutional, generational, mobile, national).

% Agricultural, construction, hospitality, and care-sector employers who draw on the deportable labor pool the regime sustains. Publicly they endorse enforcement; operationally they rely on the vulnerability it produces, and workplace sanctions are enforced at levels that leave the pool intact. They switch between documented and undocumented hiring as enforcement cycles shift, capturing the recurring wage differential.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, employers_of_deportable_labor, beneficiary,
    powerful, biographical, arbitrage, national).

% Vendors of surveillance technology, detection systems, biometric databases, detention services, and deportation logistics. Revenue scales with enforcement intensification regardless of which party governs, and contract portfolios migrate to whichever states expand enforcement next. They have no stake in the regime's normative justification, only in its budget line.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_security_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Electorates of destination states. Receive wage premia in sheltered segments, effective welfare-state closure, and the political identity of bounded membership; underwrite the regime electorally. Direct personal exposure is minimal — costs concentrate on the migrants and the displaced sectors — and their mobility is untouched by the regime they authorize. Their votes are the arrangement's ultimate source of legitimacy.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, affluent_state_citizenries, beneficiary,
    powerful, generational, mobile, national).

% Monetize the barrier itself: restriction converts human movement into a priced service, and profits scale with enforcement severity as routes shift to whatever corridor is least policed. Formally targeted by the regime, structurally parasitic on it — their entire market exists because lawful movement is suppressed. Open movement would erase their business overnight.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_smuggling_networks, beneficiary,
    organized, immediate, arbitrage, continental).

% Run legal aid, search-and-rescue operations, sanctuary networks, detention monitoring, and strategic litigation; document abuses and articulate the rights-based critique. Their members — often undocumented themselves — absorb arrests, prosecution, and removal. Disbanding is legally available and morally unavailable; from this seat the whole structure is visible at once.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migrant_rights_movements, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, migrant_rights_movements, payer).

% Treaty-monitoring committees, special rapporteurs, and regional courts that review state compliance with movement-rights provisions, issue general comments, and receive individual complaints. Findings feed domestic litigation and reputational pressure but carry no enforcement force of their own; states decline jurisdiction or ignore conclusions at negligible direct cost.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, employers_of_deportable_labor).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates territorial membership: documents identity, adjudicates entry claims, registers populations, and hands each state a single administrative lever over who may reside, work, and vote where; health and security screening at entry are coordinated centrally rather than piecemeal. Stated without evaluation: the freedom-primary reading denies that these functions justify the regime's mass-exclusion output, but the functions are real and separable in principle.
% TRANSFER_FUNCTION: Moves life prospects away from would-be crossers: wage premia and welfare closure flow to destination-state citizenries, a disciplined and deniable labor pool flows to employers, procurement revenue flows to the security industry, enforcement legitimacy flows to governing parties, and prohibition rents flow to smuggling networks — paid for by excluded migrants in restricted movement, detention, and deportability, and by domestic workers displaced through the captive-labor channel.
% ABSENT_VOICES: The excluded themselves: refused visa applicants, people intercepted at sea and land borders, detainees awaiting removal. The constituency the regime most affects holds no vote in any jurisdiction that sets its terms, and those who die in transit cannot testify at all. Their nearest proxies — diaspora communities and migrant-led organizations — stand outside the legislative chambers and diplomatic conferences where border terms are actually negotiated.
% DISAPPEARANCE_RATIONALE: Overnight disappearance of the enforcement apparatus triggers immediate labor arbitrage along global wage gradients, large migration flows toward high-wage regions, forced welfare-state redesign under new fiscal demographics, collapse of the smuggling market (whose product is the barrier itself), contraction of the border-industrial complex, and renegotiation of what citizenship membership means. Nearly every institution the regime touches reorganizes; nothing about the current configuration survives contact with open movement.
% FOUNDING_PROBLEM: After WWII, and acutely after the 1973 oil shock, destination states faced a double bind: chronic labor demand alongside firm political commitments to bounded citizenship and closed welfare. The modern regime was built to resolve this by admitting labor selectively while denying settlement — recruiting guest workers, then halting recruitment in 1973-74 while the recruited stayed, leaving the apparatus to manage permanent presence without granting permanent membership.
% FOUNDING_PROBLEM_CORROBORATION: Migration historians corroborate the founding settlement's structure from outside the benefiting parties (the standard account of the 1973 recruitment stop and its aftermath); employer federations' continuing testimony that vacancies cannot be filled attests the labor-demand half remains live; UN DESA and ILO statistical series attest the persistent wage gradients that drive movement. Only destination-state governments attest that bounded-citizenship commitments require the current scale of enforcement — no source outside the beneficiary set corroborates that the founding problem justifies today's enforcement intensity, and that silence is itself signal.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.85, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.85 for the standing border-control arrangement as this reading assesses it (referent fixed per the kernel-reading rule; values are reading-indexed over that fixed referent). Counted as extraction: mass denial of a liberty the reading holds fundamental, deaths and prolonged detention at borders, and the deportability discount on resident workers' wages. Suppression 0.88 is structural, not internalized — the regime persists through patrols, detention, deportation, visa architecture, and externalized interdiction; coercion is applied to bodies and files, not instilled as belief, so no structural-versus-internalized ambiguity omega is required beyond the standard profile. Theater 0.36: deterrence-functional enforcement coexists with symbolic infrastructure (walls on low-crossing segments, deployment choreography) whose share rises as crossings fall. Accessibility_collapse 0.62: no participant can exit the interstate system itself, since all habitable land is bordered, yet regional free-movement regimes prove the alternative is institutionally constructible — collapse is policy-made rather than natural, hence below mountain grade. Resistance 0.58: migrant-justice movements, sanctuary networks, rescue flotillas, strategic litigation, and abolitionist scholarship impose real costs. All three series run on one shared nine-point grid (1950-2025) so every metric is authored at every examined time point. Suppression_requirement is tracked deliberately because enforcement-capacity growth is this story's traced dynamic — an enforcement ratchet, not a stable backdrop: recruitment-stop hardening (1973-74), Schengen/Dublin externalization (1990s), post-2001 security fusion, Mediterranean militarization (2015 onward). Extraction accumulates monotonically across the series (T17-class signal). No oscillation is modeled because the historical record here is a ratchet, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute opposite constraints from the same structure. From destination_state_governments and affluent_state_citizenries the arrangement appears as constitutive self-government: democratic accountability requires answering the membership question, so enforcement reads as obligation rather than extraction. From excluded_migrants and undocumented_resident_workers the identical structure reads as an existential barrier backed by violence. Employers occupy a third seat — publicly endorsing enforcement while privately dependent on its product. The engine derives these divergences from the structural data (role, power, exit, scope); nothing in the authored claim adjudicates them. Scope amplifies the asymmetry: targets sit at national-to-global scope with trapped exit, so effective extraction compounds, while the diffuse beneficiary stands behind the same borders at near-zero personal exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Excluded_migrants and undocumented_resident_workers: declared victims with trapped exit, so d sits near 1.0 (full target), amplified by national/global scope. Displaced_domestic_workers: declared victims via the captive-labor channel (attribution carried by omega displaced_worker_causal_attribution), giving high d despite nominal protection. Destination_state_governments: declared beneficiaries (political capital, fee income) with agenda-setting policy mobility — low d, nudged upward only by the enforcement costs they themselves pay. Affluent_state_citizenries: diffuse beneficiaries with minimal direct exposure, near the beneficiary pole. Employers_of_deportable_labor: concentrated beneficiary with arbitrage exit — nearest the beneficiary pole. Border_security_contractors: revenue scales with enforcement intensity wherever it occurs — beneficiary-side. Human_smuggling_networks is the paradox seat: nominally targeted, declared beneficiary, because their gains are a direct product of the suppression itself (prohibition rents); d sits low despite adversarial posture. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Decomposition is the point. 'Border control' bundles a real residual coordination core — identity documentation, health screening, entry adjudication — with the mass-exclusion function this story isolates. Folding them together lets the coordination face launder the extraction (false rope); blanket condemnation erases functions that would survive any humane settlement. Reading the regime as snare prevents the first error; excluding the residual functions from this story's scope prevents the second — they warrant separate stories with their own epsilon. Genealogically, the founding settlement (managing temporary guest-worker recruitment) died with the 1973-74 recruitment stops while the apparatus persisted and re-purposed toward permanent exclusion management: the mandate-outlived-function signature, flagged via mandatrophy_resolved and matching the kink in the extraction series across 1970-1980. The R5 interview records the founding problem as contested rather than dead, so no mechanical dead-mandate zombie mismatch fires; the temporal series carries the abductive signal instead. The classification thereby guards against mislabeling in both directions — coordination-laundering and indiscriminate condemnation alike.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (freedom_primary) of the border_normative_status kernel. Which reading governs a given jurisdiction''s border arrangement, and what would the sibling readings structurally change?',
    'Reading adoption is a normative-political event, resolvable per-jurisdiction only by observing enacted doctrine: constitutional text, treaty ratification posture, controlling case law. A sovereignty_primary adoption removes excluded_migrants from the victim set entirely (exclusion becomes legitimate self-determination); a qualified_sovereignty adoption narrows victims to disproportionate-exclusion cases and reclassifies ordinary proportionate enforcement as legitimate.',
    'Effective extraction swings widely across readings over the SAME arrangement: approximately 0.85 under this reading, materially lower under qualified_sovereignty, near-floor under sovereignty_primary. Cross-reading comparison is valid only with referent and reading fixed; the disagreement is located in the default burden of justification for exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel indexicality: which reading of border_normative_status is instantiated, and the structural delta each sibling would introduce.').

omega_variable(
    epsilon_referent_discipline,
    'Is epsilon measured against the standing border-control arrangement (correct) or against this reading''s endorsed open-movement alternative (referent error)?',
    'Re-audit: every authored value must describe the existing regime as this reading sees it. Any value that improves because the alternative arrangement is imagined marks a referent slip and must be re-authored.',
    'A slipped referent collapses every advocacy reading toward zero epsilon and destroys cross-reading comparability; the corrected referent restores the high-extraction assessment of the standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_discipline, conceptual, 'Guards the fixed epsilon referent for kernel-reading stories: the referent is the standing arrangement under contest, never the endorsed alternative.').

omega_variable(
    open_borders_empirical_supports,
    'Do the empirical claims the reading''s instrumental wings rely on (wage effects, fiscal incidence, brain-drain dynamics) hold at open-movement scale?',
    'Natural experiments: EU enlargement wage studies, Mariel-type replications, IRCA legalization wage effects, fiscal incidence analyses of migrant cohorts, remittance-channel development accounting.',
    'Cannot overturn the deontological axioms (grounding_type deontological, not empirically falsifiable), but shifts persuasive force and repositions the displaced_domestic_workers seat; adverse findings would push that seat toward beneficiary-with-incidental-harm and soften coalition claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_borders_empirical_supports, empirical, 'Empirical status of the instrumental claims supporting the freedom-primary program.').

omega_variable(
    displaced_worker_causal_attribution,
    'Is domestic-worker displacement causally attributable to the regime''s captive-labor channel (deportability depressing wages and conditions), or to general labor-market forces the regime merely coexists with?',
    'Cross-region comparison of undocumented-labor density against native wages in substitutable sectors; before/after designs around legalization episodes; enforcement-surge discontinuity analysis.',
    'If attribution holds, displaced_domestic_workers stay in the victim set (the reading''s expected structural delta is honored) and the regime is shown to harm both sides of the labor market; if it fails, the victim set shrinks to migrants and undocumented workers, and the regime reads as a cleaner two-sided extraction with citizenries as net beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_worker_causal_attribution, empirical, 'Whether the third victim seat survives causal scrutiny.').

omega_variable(
    migrant_coalition_formation,
    'Can the regime''s dispersed, jurisdictionally fragmented victims convert numerical strength into organized countervailing power?',
    'Track durable transnational organizing: migrant-led unionization wins, extraterritorial franchise extension, ILO convention uptake, cross-border strike and rescue coordination, diaspora voting blocs.',
    'Materializing coalition power raises resistance and the marginal cost of suppression; sustained success pressures the arrangement toward negotiated (rope-like) settlement or open crisis, shifting computed classifications at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_coalition_formation, empirical, 'Coalition potential of the powerless victim seats (coalition check for a multi-victim snare).').

omega_variable(
    enforcement_symbolic_fraction,
    'What share of border-enforcement activity is deterrence-functional versus symbolic (infrastructure and deployments sited for visibility rather than intercept probability)?',
    'Route-displacement and apprehension statistics contrasting fortified versus unfortified segments; audit of siting decisions against crossing-volume data.',
    'Recalibrates theater_ratio; a dominant symbolic share indicates theatrical-maintenance dynamics operating inside the snare and strengthens the inertial component of the regime''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symbolic_fraction, empirical, 'Functional versus theatrical share of enforcement activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1950, border_normative_status__freedom_primary, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(bord_tr_t1960, border_normative_status__freedom_primary, theater_ratio, 1960, 0.16).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__freedom_primary, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(bord_tr_t1980, border_normative_status__freedom_primary, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.26).
narrative_ontology:measurement(bord_tr_t2000, border_normative_status__freedom_primary, theater_ratio, 2000, 0.29).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__freedom_primary, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(bord_tr_t2020, border_normative_status__freedom_primary, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(bord_tr_t2025, border_normative_status__freedom_primary, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(bord_be_t1950, border_normative_status__freedom_primary, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(bord_be_t1960, border_normative_status__freedom_primary, base_extractiveness, 1960, 0.54).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__freedom_primary, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(bord_be_t1980, border_normative_status__freedom_primary, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.71).
narrative_ontology:measurement(bord_be_t2000, border_normative_status__freedom_primary, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__freedom_primary, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(bord_be_t2020, border_normative_status__freedom_primary, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(bord_be_t2025, border_normative_status__freedom_primary, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1950, border_normative_status__freedom_primary, suppression_requirement, 1950, 0.34).
narrative_ontology:measurement(bord_su_t1960, border_normative_status__freedom_primary, suppression_requirement, 1960, 0.37).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__freedom_primary, suppression_requirement, 1970, 0.44).
narrative_ontology:measurement(bord_su_t1980, border_normative_status__freedom_primary, suppression_requirement, 1980, 0.53).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement(bord_su_t2000, border_normative_status__freedom_primary, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__freedom_primary, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(bord_su_t2020, border_normative_status__freedom_primary, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(bord_su_t2025, border_normative_status__freedom_primary, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Colloquial 'border ethics' conflates three structurally distinct normative claims with different epsilon, victim sets, and failure modes; per the epsilon-invariance principle the kernel border_normative_status decomposes into three stories. sovereignty_primary is the historical upstream (the Westphalian default whose authority claim structured the pre-rights order and is still cited to defeat the downstream readings); qualified_sovereignty is the post-WWII compromise layer; this file, freedom_primary, is the rights-fundamentalist reading downstream of UDHR/ICCPR codification. Each story links the others; epsilon is invariant within each file and incomparable across files unless the reading is held fixed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
