% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: State Sovereign Authority to Exclude Non-Members via Border Control
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_primary reading of the
 *   border_normative_status kernel. It asserts that territorial boundaries
 *   are legitimate instruments of collective self-determination and that
 *   states retain foundational authority to exclude non-members. This reading
 *   treats border exclusion as a primary expression of sovereignty, not a
 *   qualified practice subject to human rights override. The constraint
 *   coordinates a political community (citizens gain cohesion and access to
 *   shared goods through membership) while extracting heavily from excluded
 *   migrants, asylum seekers, and internally displaced persons who bear the
 *   cost of boundary maintenance without access to the decision-making that
 *   sustains it. The reading DIFFERS fundamentally from the freedom_primary
 *   and qualified_sovereignty readings in locating sovereignty at the
 *   foundation rather than at the derivative end of legitimacy.
 *
 * KEY AGENTS:
 *   - Citizen body: primary beneficiary (identity-locked, generational, organized)
 *   - State apparatus: agenda setter and secondary beneficiary (institutional, civilizational)
 *   - Excluded migrants: primary target (powerless, trapped, biographical)
 *   - Asylum seekers: secondary target (powerless, trapped, immediate)
 *   - Internally displaced persons: tertiary target (powerless, constrained, biographical)
 *   - Origin states: asymmetrically positioned payers and agenda setters (moderate, generational)
 *   - International human rights bodies: structurally excluded observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "State Sovereign Authority to Exclude Non-Members via Border Control").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'fc60484b-2e89-42ab-b45c-7a9c25789774').
narrative_ontology:cs_kernel_codification('fc60484b-2e89-42ab-b45c-7a9c25789774', distributed).
narrative_ontology:cs_authority_grounding('fc60484b-2e89-42ab-b45c-7a9c25789774', extraction).
narrative_ontology:cs_interpretation_layer_present('fc60484b-2e89-42ab-b45c-7a9c25789774').
narrative_ontology:cs_reading_relation('fc60484b-2e89-42ab-b45c-7a9c25789774', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('fc60484b-2e89-42ab-b45c-7a9c25789774', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('fc60484b-2e89-42ab-b45c-7a9c25789774', foundational, territorial_membership_foundational).
narrative_ontology:cs_axiom_status(territorial_membership_foundational, holdable).
narrative_ontology:cs_axiom_grounding('fc60484b-2e89-42ab-b45c-7a9c25789774', territorial_membership_foundational, deontological).
narrative_ontology:cs_axiom('fc60484b-2e89-42ab-b45c-7a9c25789774', foundational, state_sovereign_exclusion_authority).
narrative_ontology:cs_axiom_status(state_sovereign_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding('fc60484b-2e89-42ab-b45c-7a9c25789774', state_sovereign_exclusion_authority, deontological).
narrative_ontology:cs_reference_frame('fc60484b-2e89-42ab-b45c-7a9c25789774', westphalian_sovereignty_system).
narrative_ontology:cs_drift_state('fc60484b-2e89-42ab-b45c-7a9c25789774', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fc60484b-2e89-42ab-b45c-7a9c25789774', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_body).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, internally_displaced_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, origin_states).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, territorial_integrity_principle).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizenship confers membership in the political community that claims sovereign authority to set the boundary. Citizens benefit from the exclusion in the form of preferential access to state services, labor markets, and political voice. Their identity is constituted through membership, and exit would require renouncing that membership. The constraint treats citizen displacement (within the national territory) as outside its scope entirely — only non-member exclusion registers as the constraint's legitimate function.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_body, beneficiary,
    organized, generational, identity_locked, national).

% Administers and enforces the boundary through immigration law, border patrol, deportation machinery, and visa regimes. Justifies exclusion as necessary to maintain social cohesion, welfare state sustainability, and political community integrity. Controls the definition of membership and the criteria for entry. Collects sovereignty rent — the ability to allocate scarce membership status — and channels enforcement resources toward barrier maintenance.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, civilizational, mobile, national).

% Persons outside the territorial boundary who seek entry but are denied it by force or law. They bear the cost of exclusion: separation from economic opportunity, inability to access state services, exposure to violence or precarious labor in origin countries, and legal barriers to movement. The constraint treats their exclusion as legitimate state function; their objections are framed as external to the political community whose self-determination is being protected. No arbitrage available — moving to another state simply relocates the boundary that excludes them.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Persons fleeing persecution, violence, or deprivation who present themselves at borders claiming refuge. The constraint subordinates humanitarian obligations to sovereign discretion: states retain authority to exclude asylum seekers when they assess it serves their political or economic interests. Asylum seekers are doubly powerless — unable to exit the originating danger and unable to obtain entry where they seek safety. Their situation exemplifies the constraint's asymmetry: the state that denies entry faces no reciprocal obligation.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Citizens or long-term residents displaced within the national territory (by war, environmental catastrophe, development) who require relocation but have no recognized right to move across borders to seek opportunity or safety abroad. The constraint treats their displacement as a domestic governance problem, not a boundary-definition problem — their exclusion from options beyond the border is treated as uncontroversial, even when the state that displaced them cannot or will not accommodate them domestically. Their exit options are bounded by the state's capacity to provide, not by any claim on neighboring states' territory.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, internally_displaced_persons, payer,
    powerless, biographical, constrained, national).

% States where excluded persons originate, whose citizens and residents face barriers to exit their own territory or to cross international boundaries. These states have sovereignty claims of their own but typically lack enforcement capacity to prevent emigration. The constraint they face is asymmetric: receiving states exercise exclusion unilaterally, while origin states must manage the domestic consequences of exclusion (pressure from those unable to leave, brain drain, remittance dependence) without reciprocal influence over receiving-state borders.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, origin_states, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, origin_states, agenda_setter).

% UN bodies, treaty bodies, and regional human rights mechanisms that invoke freedom of movement and proportionality doctrines would, if seated at the decision table, contest the constraint's framing by asserting that sovereignty is not absolute and must accommodate human rights claims. Their exclusion from the enforcement machinery means their judgments can be overruled by states invoking sovereign immunity. They would argue for qualified rather than primary sovereignty, but their voice is structurally excluded from the constraint's operation.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a bounded political community within which collective self-determination, democratic deliberation, and welfare provision become feasible. Citizens within the boundary can coordinate on shared institutions, laws, and resource distribution; the boundary is presented as logically prior to any such coordination—without it, the 'people' has no defined membership and cannot act collectively.
% TRANSFER_FUNCTION: Transfers the ability to migrate, claim welfare state access, and participate in labor markets from excluded persons to the citizen body. The state apparatus collects enforcement rents—resources devoted to border patrol, immigration law, and deportation. Citizens collect preferential access to scarce goods (employment, education, public benefits) by virtue of membership status.
% ABSENT_VOICES: Excluded migrants and asylum seekers have no seat at the table where the boundary is set or where its legitimacy is debated. Origin states have limited influence. International human rights bodies are structurally excluded from enforcement decisions. Those who would argue that freedom of movement overrides state sovereignty, or that sovereignty must be qualified by human rights obligations, are outside the authority structure that sustains this reading of the constraint.
% DISAPPEARANCE_RATIONALE: If this constraint—the principle that states have foundational authority to exclude non-members—vanished overnight, global labor markets would reorganize around movement rather than stasis; welfare states would either collapse or reconstitute around membership redefined (planetary, functional, or resource-based rather than territorial); and the nation-state system's current form would be unrecognizable. The constraint is not a description of inevitable politics but a legally and institutionally maintained arrangement.
% FOUNDING_PROBLEM: How can a bounded political community maintain internal democracy, rule of law, and resource distribution without the ability to control who is admitted to membership? Early modern political theory posed this as: How can the sovereign people govern themselves without borders?
% FOUNDING_PROBLEM_CORROBORATION: State apparatus and mainstream international law doctrine attest the problem is live and that borders are the solution. Excluded migrants, humanitarian advocates, and freedom-of-movement readings attest the problem is either solved (through supranational governance, humanitarian norms, and labor mobility) or was never as binding as sovereignty doctrine claims. International human rights bodies attest that the founding problem can be addressed through proportional, rights-consistent border management rather than absolute state discretion. No consensus exists outside the benefiting institutional structure.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers scarce goods (territorial access, membership status, labor market participation) from powerless excluded persons to citizens and the state. However, it is not at the snare maximum (0.85+) because the constraint does maintain a real coordination function for the citizen body—boundaries are not pure extraction. Suppression (0.72) is substantial and rising because the constraint's persistence depends on active enforcement: immigration law, border patrol, detention, deportation, and visa regimes all work to exclude those seeking entry. The growth from 0.48 to 0.72 over the interval reflects historical intensification of border enforcement capacity in wealthy democracies. Theater_ratio (0.41) is moderate and rising because enforcement activity increasingly emphasizes security framing ('national security', 'border security', 'protecting citizens from threats') where the primary function is actually economic exclusion—protecting labor market access for citizens by excluding cheaper workers. The trajectory shows boundary securitization (framing as threat-response rather than preference-based collective choice) increasing over time. Accessibility_collapse (0.79) is high because once borders are understood as legitimate sovereign instruments, alternatives collapse: the excluded cannot realistically seek entry legally, cannot exit their origin conditions easily, and have no recourse to international enforcement of movement rights. Resistance (0.58) is moderate because excluded migrants mount real pressure (asylum applications, irregular crossings, humanitarian advocacy) but lack institutional voice or enforcement capacity to change the constraint itself. The measurement series is authored on one shared grid: every metric is valued at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: from the citizen and state-apparatus seats, it is legitimate collective self-determination, a rope coordinating a political community. From the excluded migrant and asylum seeker seats, it is coercive extraction backed by force, a snare. From the origin-state seat, it is a tangled structure that extracts from their populations while denying them voice in the receiving state's decisions. The engine computes these different classifications per-seat from the structural data; the sovereignty_primary reading as a whole is authored as tangled_rope because the coordination function (benefiting citizens) is real but inseparable from the extraction function (targeting excluded persons). This is precisely the mandatrophy question: does the constraint coordinate citizens by extracting from the excluded, or does it extract from the excluded while framing the extraction as necessary coordination? The readings diverge on this question. The sovereignty_primary reading says coordination justifies extraction; the freedom_primary reading says extraction delegitimizes the coordination claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizen body benefits structurally from exclusion (preferential access to labor, benefits, political voice) but is identity-locked into the state, giving them high directionality variance: from their own seat they experience low extraction (they benefit), but from the excluded migrant's seat they appear as the collective agent extracting the cost. The state apparatus is the primary extractor, with directionality near the target end (it runs the enforcement machinery and collects sovereignty rents). The excluded migrants sit at full-target directionality (d ≈ 1.0): they bear all costs and have no accessible exit. Asylum seekers face the same directionality as excluded migrants. Internally displaced persons have constrained but not trapped exit (they could theoretically leave the state if the state did not control borders), giving them directionality slightly lower than excluded migrants. Origin states are caught in a structural inversion: they have sovereign authority over their own borders (moderate power) but are powerless to influence receiving-state boundaries that exclude their citizens, creating a directionality bind (they pay through brain drain and remittance dependence without controlling the extraction). No overrides are needed; the derivation chain (beneficiary + identity_locked → low d; victim + powerless + trapped → high d) captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic tangled_rope mandatrophy: it solves a genuine coordination problem (how to maintain a bounded political community capable of collective action) by imposing costs on a third party (excluded persons) who have no seat at the decision table. The distinction from pure snare is that the coordination function is not fictional—citizens do benefit from boundary maintenance in real ways (labor market protection, political community, welfare allocation). The distinction from pure rope is that the extraction from excluded persons is not incidental or proportional—it is structural and asymmetric. The constraint persists because beneficiaries (citizens) are numerous and concentrated enough (organized power) to defend it, while victims (excluded persons) are dispersed across multiple countries and completely powerless, facing massive barriers to coalition-building or exit. The founding problem (how to sustain democratic self-determination in a bounded political community) remains live for the beneficiary seat but is treated as already solved (by exclusion) from the beneficiary's perspective. The founding_problem_status = contested accurately captures this: the state apparatus and citizens assert the problem is ongoing and sovereignty is the answer; the excluded and freedom-of-movement advocates assert the problem was overspecified (assuming bounds are necessary rather than contingent) and was solved long ago.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_necessity,
    'Is the founding problem (how to sustain bounded democratic self-determination) structurally unsolvable without territorial exclusion, or can it be addressed through alternative membership definitions (functional, diaspora-inclusive, transnational)?',
    'Natural experiments in supranational governance (EU, MERCOSUR), digital community membership, and non-territorial organizing; investigation of whether political participation and welfare distribution can be decoupled from territorial presence.',
    'If the problem is solvable without exclusion, the constraint''s founding rationale is contingent rather than necessary, reclassifying it as a discretionary extraction riding on a solved coordination need (snare rather than tangled_rope). If territorial exclusion is genuinely necessary, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_necessity, conceptual, 'Whether territorial boundaries are structurally necessary for the founding problem or contingent on particular political choices.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of migrant exit primarily structural (legal barriers, geographic isolation, economic desperation) or partially internalized (migrant acceptance of exclusion as legitimate sovereign right)?',
    'Comparative analysis of migrant behavior when legal barriers are removed (European visa liberalization, internal open-border agreements); post-exit suppression trajectory (do migrants who manage entry continue to accept border legitimacy, or does suppression reverse upon exit?).',
    'If suppression is primarily structural, the 0.72 score reflects coercive enforcement. If substantially internalized through acceptance of sovereignty doctrine, the effective suppression is higher—the constraint travels with the migrant beyond the border. If partially internalized, suppression should be decomposed into structural and internalized components for future measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression reflects external coercion or partially internalized acceptance of sovereignty legitimacy.').

omega_variable(
    citizen_identity_lock_contingency,
    'Is citizen identity-lock genuine (citizenship is constitutive of identity, exit means existential loss) or contingent (citizens could realistically adopt alternative political identity)?',
    'Ethnographic and historical analysis of citizenship renunciation; study of naturalization and political identity in diaspora; investigation of whether stateless persons report identity loss or identity reconstruction.',
    'If identity-lock is genuine and universal across citizens, their directionality is locked at low extraction (they cannot exit even if exploited). If contingent or unevenly distributed (some citizens could exit, others are locked), then directionality varies and some citizens approximate moderate or high d, reclassifying them from beneficiary to partially trapped. This would lower the claim that the constraint uniformly benefits citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_identity_lock_contingency, empirical, 'Whether citizen membership is constitutively identity-locking or contingently so.').

omega_variable(
    origin_state_complicity,
    'To what degree do origin states actively collaborate with receiving states to enforce exclusion (deportation agreements, border security assistance) versus passively accept it?',
    'Analysis of bilateral and multilateral agreements on migration control; investigation of state capacity to enforce emigration bans and whether they do; study of border security assistance and deportation procedures.',
    'If origin states actively collaborate, they transition from payer to secondary agenda-setter (they help enforce the extraction they are paying). If they resist and are overridden, the extraction is more unilateral and less tangled. This affects whether the constraint is sustainable (collaboration = yes; resistance = long-term pressure for change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_state_complicity, empirical, 'Whether origin state complicity in enforcement affects the constraint''s sustainability and classification.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the sovereignty_primary reading logically foreclose the freedom_primary reading within a single coherent authority framework, or do they coexist as competing live readings held by different institutional seats?',
    'Analysis of constitutional doctrine, international law, and institutional practice across jurisdictions; identification of any unified legal or political framework that successfully maintains both readings without contradiction.',
    'If readings foreclose each other, the classification of one as Tangled Rope and the other as Snare is the correct measure of structural incompatibility. If they coexist, both readings are live in the global political system, and the classification differences reflect institutional seat location rather than one reading being replaced by the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether sovereignty_primary and freedom_primary readings are structurally incompatible or can coexist across different authority frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__sovereignty_primary, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(bord_tr_t8, observed).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__sovereignty_primary, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(bord_tr_t16, observed).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__sovereignty_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(bord_tr_t24, observed).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__sovereignty_primary, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(bord_tr_t32, observed).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__sovereignty_primary, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(bord_tr_t40, observed).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__sovereignty_primary, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(bord_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t8, border_normative_status__sovereignty_primary, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(bord_be_t8, observed).
narrative_ontology:measurement(bord_be_t16, border_normative_status__sovereignty_primary, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(bord_be_t16, observed).
narrative_ontology:measurement(bord_be_t24, border_normative_status__sovereignty_primary, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(bord_be_t24, observed).
narrative_ontology:measurement(bord_be_t32, border_normative_status__sovereignty_primary, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(bord_be_t32, observed).
narrative_ontology:measurement(bord_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(bord_be_t40, observed).
narrative_ontology:measurement(bord_be_t50, border_normative_status__sovereignty_primary, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(bord_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t8, border_normative_status__sovereignty_primary, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(bord_su_t8, observed).
narrative_ontology:measurement(bord_su_t16, border_normative_status__sovereignty_primary, suppression_requirement, 16, 0.63).
narrative_ontology:measurement_basis(bord_su_t16, observed).
narrative_ontology:measurement(bord_su_t24, border_normative_status__sovereignty_primary, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(bord_su_t24, observed).
narrative_ontology:measurement(bord_su_t32, border_normative_status__sovereignty_primary, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(bord_su_t32, observed).
narrative_ontology:measurement(bord_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(bord_su_t40, observed).
narrative_ontology:measurement(bord_su_t50, border_normative_status__sovereignty_primary, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(bord_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_normative_status__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Part of the border_normative_status kernel family. The sovereignty_primary reading frames territory and membership as foundation for collective self-determination; freedom_primary reads freedom of movement as primary with sovereignty derivative; qualified_sovereignty posits both as binding constraints requiring proportional balance. All three readings operate on the same kernel (what legitimacy grounds state border authority) but decompose extractiveness and beneficiary/victim sets differently. Network links enable contamination analysis: if one reading's institutional authority erodes, the others shift in their operational environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
