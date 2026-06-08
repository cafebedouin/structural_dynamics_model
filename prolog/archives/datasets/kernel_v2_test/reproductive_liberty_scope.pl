% ============================================================================
% CONSTRAINT STORY: reproductive_liberty_scope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reproductive_liberty_scope, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reproductive_liberty_scope
 *   human_readable: Reproductive Liberty Scope: Does Germline Genetic Modification Fall Within Parental Autonomy?
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The reproductive liberty scope constraint addresses whether germline
 *   genetic modification (GGM) falls within the established domain of
 *   reproductive autonomy or exceeds its legitimate boundaries. This is
 *   structurally distinct from debates about whether reproductive liberty is
 *   valuable (that question is upstream, modeled in
 *   genetic_parenthood_valuation). The constraint exhibits tangled rope
 *   dynamics: it genuinely coordinates parental reproductive choices by
 *   providing a legal and ethical framework, while simultaneously extracting
 *   from future children (who cannot consent to genetic modifications that
 *   define their biological substrate), the genetic commons (through
 *   selection pressures that reduce diversity), and disability advocacy
 *   communities (by normalizing the framing of disability as preventable
 *   harm). The 16-to-5 article ratio in bioethics literature reflects that
 *   the liberty-expansion argument is structurally favored in contemporary
 *   discourse, but the minority position identifies real asymmetric
 *   extraction that the majority framing treats as coordination. The
 *   constraint's extractiveness has increased over the 30-year interval (0.28
 *   to 0.48) as genetic technologies have advanced from hypothetical to
 *   clinically available, making the scope question urgent rather than
 *   abstract. Suppression has also increased (0.45 to 0.62) as the
 *   liberty-maximizing framework has become institutionally entrenched,
 *   making it harder to articulate limits without being framed as anti-choice
 *   or paternalistic.
 *
 * KEY AGENTS:
 *   - Future Children: Primary victim (powerless/identity_locked) — ontologically bound to genetic choices made before their existence; cannot consent or exit
 *   - Intended Parents: Primary beneficiary (institutional/arbitrage) — gain expanded reproductive options through liberty-expansion framing
 *   - Disability Advocacy Communities: Secondary victim (moderate/constrained) — threatened by normalization of genetic selection against disability traits, but also protected by reproductive liberty when it defends disabled parents' right to reproduce
 *   - Fertility Industry: Secondary beneficiary (institutional/constrained) — gains market access but faces regulatory uncertainty and reputational risk
 *   - Genetic Commons: Abstract victim (powerless/trapped) — collective good with no advocate; bears cost of diversity loss from selection pressures
 *   - International Bioethics Governance Coalition: Organized actors (organized/mobile) — building alternative governance pathways with implicit sunset on current permissive regime
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and asymmetric extraction; tangled rope classification reflects irreducible structural ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reproductive_liberty_scope, 0.48).
domain_priors:suppression_score(reproductive_liberty_scope, 0.62).
domain_priors:theater_ratio(reproductive_liberty_scope, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reproductive_liberty_scope, extractiveness, 0.48).
narrative_ontology:constraint_metric(reproductive_liberty_scope, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reproductive_liberty_scope, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reproductive_liberty_scope, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(reproductive_liberty_scope, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reproductive_liberty_scope, tangled_rope).
narrative_ontology:human_readable(reproductive_liberty_scope, "Reproductive Liberty Scope: Does Germline Genetic Modification Fall Within Parental Autonomy?").
narrative_ontology:topic_domain(reproductive_liberty_scope, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:requires_active_enforcement(reproductive_liberty_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reproductive_liberty_scope, intended_parents).
narrative_ontology:constraint_beneficiary(reproductive_liberty_scope, fertility_industry).
narrative_ontology:constraint_beneficiary(reproductive_liberty_scope, genetic_counseling_profession).
narrative_ontology:constraint_victim(reproductive_liberty_scope, future_children).
narrative_ontology:constraint_victim(reproductive_liberty_scope, genetic_commons).
narrative_ontology:constraint_victim(reproductive_liberty_scope, disability_advocacy_communities).
narrative_ontology:constraint_vindicates(reproductive_liberty_scope, reproductive_autonomy_doctrine).
narrative_ontology:constraint_vindicates(reproductive_liberty_scope, parental_authority_over_offspring).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE CHILD (SNARE) — Identity-locked rather than trapped because the binding mechanism is ontological: the future child's identity is constituted through the genetic choices made before their existence. They cannot exit a constraint that defines their biological substrate. The constraint extracts from them by foreclosing alternative genetic configurations they might have preferred, with no mechanism for consent or revision. Maximum extraction because the agent has no voice in the decision that structures their embodied existence.
constraint_indexing:constraint_classification(reproductive_liberty_scope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(universal))).

% PERSPECTIVE 2: DISABILITY ADVOCACY COMMUNITY (TANGLED ROPE) — Constrained by cultural momentum toward genetic optimization and by the framing of disability as preventable harm. Benefits from the reproductive liberty framework when it protects disabled parents' right to reproduce, but is victimized when the same framework is extended to justify selecting against disability traits. Mixed coordination and extraction: the liberty principle both protects and threatens this community.
constraint_indexing:constraint_classification(reproductive_liberty_scope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTENDED PARENTS (ROPE) — Primary beneficiaries with arbitrage-level exit (can choose providers, jurisdictions, or opt out entirely). Experience the constraint as coordination: the reproductive liberty framework enables them to pursue genetic modification by framing it as an extension of existing parental rights. Low effective extraction because the constraint runs in their favor.
constraint_indexing:constraint_classification(reproductive_liberty_scope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FERTILITY INDUSTRY (TANGLED ROPE) — Benefits from expanded scope of reproductive liberty (more services to offer, higher revenue) but is constrained by regulatory uncertainty and reputational risk. The industry coordinates with intended parents to deliver services but extracts from the genetic commons by normalizing commodified genetic selection. Mixed beneficiary/victim status: gains market access but faces compliance costs and ethical scrutiny.
constraint_indexing:constraint_classification(reproductive_liberty_scope, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL BIOETHICS GOVERNANCE COALITION (SCAFFOLD) — Organized actors (WHO, UNESCO, national bioethics councils) see the current liberty-maximizing framework as a temporary coordination mechanism while international consensus on limits is negotiated. The coalition is building alternative governance pathways (international treaties, harmonized regulations, oversight bodies) with an implicit sunset: once global norms mature, the current permissive regime will be replaced by bounded reproductive liberty with explicit limits on heritable modifications.
constraint_indexing:constraint_classification(reproductive_liberty_scope, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (resolving disputes about parental authority, enabling reproductive choices) and asymmetric extraction (foreclosing future persons' genetic autonomy, normalizing eugenic selection pressures). The 16-to-5 article ratio reflects that the liberty-expansion argument is structurally favored in contemporary bioethics discourse, but the minority position identifies real extraction that the majority framing obscures. Tangled rope because both functions are structurally present and neither can be eliminated without dissolving the constraint.
constraint_indexing:constraint_classification(reproductive_liberty_scope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reproductive_liberty_scope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reproductive_liberty_scope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reproductive_liberty_scope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reproductive_liberty_scope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reproductive_liberty_scope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from future children by foreclosing genetic configurations they might have preferred (no consent mechanism), from the genetic commons by normalizing selection pressures that reduce diversity, and from disability communities by framing disability as preventable harm. But extraction is not maximal because the constraint also provides genuine coordination: it resolves disputes about parental authority, enables reproductive choices that many parents value, and protects some reproductive freedoms (e.g., disabled parents' right to reproduce). The value reflects that roughly half the constraint's operation is extractive and half is coordinative. Suppression (0.62): Moderate-high. Significant barriers to challenging the liberty-expansion framing include: the cultural momentum toward genetic optimization, the institutional entrenchment of reproductive autonomy doctrine in law and bioethics, the economic interests of the fertility industry, and the rhetorical difficulty of articulating limits without being framed as paternalistic or anti-choice. But suppression is not total: the 5-article minority position exists, international governance efforts are building alternative frameworks, and disability advocacy communities are articulating coherent critiques. Theater ratio (0.38): Moderate. Some performative elements exist (ethics committees that rubber-stamp procedures, consent forms that cannot meaningfully inform about long-term heritable effects, regulatory oversight that focuses on procedural compliance rather than substantive limits), but the constraint is not primarily theatrical. Most of the coordination and extraction functions are real rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The future child sees pure extraction (Snare) because they are identity-locked into genetic choices made before their existence with no consent mechanism. The intended parents see coordination (Rope) because the liberty framework enables their reproductive goals. The disability advocacy community sees mixed coordination and extraction (Tangled Rope) because the same liberty principle both protects and threatens them. The fertility industry sees mixed benefits and constraints (Tangled Rope) because expanded liberty creates market opportunities but also regulatory risk. The international governance coalition sees a temporary coordination mechanism with a sunset (Scaffold) because they are building alternative frameworks. The analytical observer sees irreducible structural ambiguity (Tangled Rope) because both coordination and extraction are genuinely present and neither can be eliminated without dissolving the constraint. The perspectival gap is not resolvable by better measurement — it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Intended parents are declared beneficiaries with arbitrage exit, yielding low d (near 0.0) and negative or near-zero effective extraction — they experience the constraint as net benefit. Future children are declared victims with identity_locked exit, yielding high d (near 1.0) and maximum effective extraction — they bear the full cost with no exit. Disability advocacy communities are declared victims with constrained exit, yielding moderate-high d (0.6-0.7 range) — they experience substantial extraction but have some agency to resist through advocacy and coalition-building. The fertility industry is declared as both beneficiary and victim (mixed status) with constrained exit, yielding moderate d (0.4-0.5 range) — they experience the constraint as both opportunity and burden. The genetic commons is an abstract victim with trapped exit, yielding high d similar to future children. The analytical observer uses the analytical exit option, which produces d based on the structural balance of coordination vs extraction in the base properties, independent of personal stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope is the structurally accurate classification when both coordination and extraction are irreducibly present. The 16-to-5 article ratio does not indicate that one side is correct and the other mistaken — it indicates that the liberty-expansion framing is structurally favored in contemporary bioethics discourse (beneficiaries have more institutional power than victims). The minority position is not wrong; it is identifying real extraction that the majority framing treats as coordination. The mandate (reproductive liberty) has not outlived its function, but its scope has expanded beyond the domain where coordination clearly dominates extraction. The constraint is not a false summit (it is not a mountain being naturalized) but a genuine hybrid where the coordination function (enabling parental reproductive choices) and the extraction function (foreclosing future persons' genetic autonomy) are structurally inseparable. Attempting to eliminate the extraction (by prohibiting all genetic modification) would also eliminate the coordination (parental reproductive autonomy). Attempting to eliminate the coordination framing (by treating all genetic modification as pure extraction) would foreclose legitimate parental choices. The tangled rope classification captures this irreducible structural tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_person_moral_status,
    'Do future persons who do not yet exist have moral claims that constrain present reproductive choices, or does moral status require actual existence?',
    'Philosophical consensus on personhood thresholds; legal precedents on duties to future generations; empirical data on long-term outcomes of genetic modification',
    'If future persons have strong moral claims: reproductive liberty scope must be bounded (snare classification gains support). If moral status requires existence: liberty-expansion argument is strengthened (rope classification from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_person_moral_status, conceptual, 'Whether future persons have moral claims constraining present reproductive choices').

omega_variable(
    liberty_scope_natural_boundary,
    'Is there a natural boundary to reproductive liberty (e.g., at the threshold of heritable modifications), or is the scope entirely a matter of social construction and policy choice?',
    'Cross-cultural analysis of reproductive norms; historical analysis of how liberty boundaries have shifted; identification of stable vs contingent limits',
    'If natural boundary exists: some version of the constraint is a mountain (immutable limit). If entirely constructed: the constraint is pure policy choice (rope or snare depending on whose interests prevail).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberty_scope_natural_boundary, conceptual, 'Whether reproductive liberty has natural boundaries or is entirely constructed').

omega_variable(
    genetic_commons_depletion_rate,
    'At what rate does widespread genetic selection deplete the genetic commons (diversity, rare alleles, non-optimized traits), and does this depletion create irreversible harm?',
    'Population genetics modeling; longitudinal tracking of allele frequencies in populations with high IVF+PGD usage; assessment of phenotypic diversity loss',
    'If depletion is rapid and irreversible: extraction from genetic commons is severe (snare from commons perspective). If depletion is slow or reversible: coordination benefits may outweigh commons costs (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_commons_depletion_rate, empirical, 'Rate and reversibility of genetic commons depletion from selection practices').

omega_variable(
    disability_community_threshold,
    'At what threshold of genetic selection does the reproductive liberty framework shift from protecting disabled parents'' reproductive rights to threatening the disability community''s existence?',
    'Empirical tracking of selection rates against disability-associated traits; qualitative research on disability community experiences; analysis of when protection becomes threat',
    'If threshold is low (already crossed): tangled rope classification for disability community is confirmed. If threshold is high (not yet reached): current regime may still be protective rope from their perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disability_community_threshold, empirical, 'Threshold at which liberty framework shifts from protection to threat for disability community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reproductive_liberty_scope, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(replib_theater_1990, reproductive_liberty_scope, theater_ratio, 0, 0.25).
narrative_ontology:measurement(replib_theater_2000, reproductive_liberty_scope, theater_ratio, 10, 0.3).
narrative_ontology:measurement(replib_theater_2010, reproductive_liberty_scope, theater_ratio, 20, 0.35).
narrative_ontology:measurement(replib_theater_2020, reproductive_liberty_scope, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(replib_extract_1990, reproductive_liberty_scope, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(replib_extract_2000, reproductive_liberty_scope, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(replib_extract_2010, reproductive_liberty_scope, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(replib_extract_2020, reproductive_liberty_scope, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(replib_suppress_1990, reproductive_liberty_scope, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(replib_suppress_2000, reproductive_liberty_scope, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(replib_suppress_2010, reproductive_liberty_scope, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(replib_suppress_2020, reproductive_liberty_scope, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reproductive_liberty_scope, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of genetic_parenthood_valuation (which establishes that genetic connection is valued) and informed_consent_impossibility (which establishes that future persons cannot consent to genetic modifications). The reproductive_liberty_scope constraint takes those upstream facts as given and addresses the distinct question of whether GGM falls within or exceeds the boundaries of reproductive autonomy. The extractiveness values are independent: genetic_parenthood_valuation has low extraction (rope) because valuing genetic connection is largely coordinative; informed_consent_impossibility has negligible extraction (mountain) because it is a logical limit; reproductive_liberty_scope has moderate-high extraction (tangled rope) because extending liberty to cover GGM creates asymmetric costs for future persons and the genetic commons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
