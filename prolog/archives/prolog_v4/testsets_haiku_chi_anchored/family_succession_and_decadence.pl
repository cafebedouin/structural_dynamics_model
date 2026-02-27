% ============================================================================
% CONSTRAINT STORY: family_succession_and_decadence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_succession_and_decadence, []).

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
 *   constraint_id: family_succession_and_decadence
 *   human_readable: Meiji-Taisho Family Succession and the 'Ie' System
 *   domain: social/familial
 *
 * SUMMARY:
 *   The Meiji-Taisho ie (family house) system represents a legal and cultural
 *   architecture for concentrating property, labor, and reproductive
 *   authority within patrilineal succession lineages. Codified in the 1873
 *   Meiji civil law and reinforced through state conscription and tax
 *   collection, the ie system extracted resources from younger sons
 *   (conscription, forced occupational specialization), daughters (bride
 *   price transfers, productive/reproductive labor), and daughters-in-law
 *   (ritual subordination, property transfer through childbearing).
 *   Jun'ichirō Tanizaki's 1920 short story 'Atsumono' (A Gourmet Club)
 *   depicts this system through the lens of a younger son trapped in
 *   subordinate status, illustrating both the structural coercion and the
 *   performative theater through which ie obligations are enacted. The
 *   constraint combines genuine coordination (house perpetuation requires
 *   property consolidation and transgenerational authority) with severe
 *   extraction (benefits concentrate in the patriarch and primary heir while
 *   costs distribute across younger siblings and all women). The theater
 *   ratio (0.68) reflects increasing formalization and ritualization of ie
 *   obligations as modernization creates exit options—the system becomes more
 *   performative precisely as its actual binding force weakens.
 *
 * KEY AGENTS:
 *   - Patriarch/Primary Heir: Primary beneficiary (institutional/arbitrage) — controls succession, property, marriage allocation, family labor; experiences ie as coordination mechanism enabling house perpetuation
 *   - Younger Sons: Primary victims (powerless/trapped) — systematically excluded from inheritance; forced into military service, priesthood, or commercial dependence; cannot exit family obligation bonds without shame and economic ruin
 *   - Daughters: Primary victims (powerless/trapped) — excluded from inheritance; marriage functions as transfer of ie obligations to another family; no self-determination in spouse selection; economic dependence through entire lifecycle
 *   - Daughters-in-Law: Primary victims (powerless/trapped) — enter new ie under absolute patriarch/mother-in-law authority; trapped within marital house through economic and social dependence; reproduce ie constraints on subsequent generations
 *   - State/Meiji Government: Institutional actor (institutional/arbitrage) — codified ie in civil law; used ie system as primary unit for taxation and conscription; benefits from centralized authority structure; maintains ie through legal enforcement but increasingly through theater as enforcement capacity declines
 *   - Extended kin networks: Secondary actors — younger brothers, uncles, aunts occupy constrained positions; serve as fallback labor or care roles for house; participate in ritual performance of ie hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_succession_and_decadence, 0.58).
domain_priors:suppression_score(family_succession_and_decadence, 0.72).
domain_priors:theater_ratio(family_succession_and_decadence, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_succession_and_decadence, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_succession_and_decadence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_succession_and_decadence, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_succession_and_decadence, tangled_rope).
narrative_ontology:human_readable(family_succession_and_decadence, "Meiji-Taisho Family Succession and the 'Ie' System").
narrative_ontology:topic_domain(family_succession_and_decadence, "social/familial").

domain_priors:requires_active_enforcement(family_succession_and_decadence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_succession_and_decadence, house_patriarch).
narrative_ontology:constraint_beneficiary(family_succession_and_decadence, primary_heir).
narrative_ontology:constraint_victim(family_succession_and_decadence, younger_sons).
narrative_ontology:constraint_victim(family_succession_and_decadence, daughters).
narrative_ontology:constraint_victim(family_succession_and_decadence, daughters_in_law).
narrative_ontology:constraint_victim(family_succession_and_decadence, family_members_without_inheritance_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNGER SON (SNARE) — Structurally excluded from house succession. Cannot exit family obligations (ie codes bind all members). Forced into military service, priesthood, or subordinate merchant roles. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.57. Maximum extraction with no alternatives.
constraint_indexing:constraint_classification(family_succession_and_decadence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DAUGHTER (SNARE) — Trapped by patrilineal succession rules. Marriage is a transfer of ie obligations, not a choice. Economic dependence on family for bride price and inheritance contingent on producing male heir. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.65. Extreme extraction with no self-determination.
constraint_indexing:constraint_classification(family_succession_and_decadence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DAUGHTER-IN-LAW (SNARE) — Enters new ie under complete authority of mother-in-law and patriarch. Subject to elaborate ritual deference (the ie reproduces patriarchal extraction across generations). Trapped within marital house; divorce brings shame and economic ruin. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.62. Enters as victim and enforces victimization of future daughters-in-law.
constraint_indexing:constraint_classification(family_succession_and_decadence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRIMARY HEIR / ELDEST SON (TANGLED ROPE) — Benefits from inheritance and house authority but constrained by absolute obligation to perpetuate ie (cannot dissolve, divide, or reform the house). Must manage younger siblings' resentment, produce heirs, maintain ritual performance. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39. Mixed: coordination benefit (house continuity) + asymmetric extraction (subordinate family members).
constraint_indexing:constraint_classification(family_succession_and_decadence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PATRIARCH / HOUSE INSTITUTION (ROPE) — Benefits maximally from ie system. Controls succession, property allocation, member labor, daughter marriages. But also enforces ritual obligations and custodianship. Experiences constraint as coordination mechanism: ie rules enable house perpetuation and resource concentration. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary; extraction scales downward.
constraint_indexing:constraint_classification(family_succession_and_decadence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE LEGAL APPARATUS (PITON) — Codified ie system in civil law (1873 family register system, patrilineal succession law). Maintained ie as primary tax and conscription unit. But by 1920s, actual enforcement relies on family theater (ritual deference, shame-based compliance) more than legal coercion. state interest in ie is degrading as modernization creates exit options. theater_ratio=0.68 (high performativity; legal form persists despite declining enforcement capacity). Institutional inertia: ie persists through legal code maintenance, not because state actively enforces it.
constraint_indexing:constraint_classification(family_succession_and_decadence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risk of analyzing ie system as an immutable feature of Japanese culture or family structure. Temptation to treat patrilineal succession as inherent to kinship systems. However, structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts mountain classification. The engine's false summit detector will reveal that the 'natural' or 'traditional' framing actually naturalizes a contingent institutional arrangement optimized for property concentration and labor extraction.
constraint_indexing:constraint_classification(family_succession_and_decadence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_succession_and_decadence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_succession_and_decadence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_succession_and_decadence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_succession_and_decadence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_succession_and_decadence, TR),
    TR >= 0.70.

:- end_tests(family_succession_and_decadence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The patriarch and primary heir capture disproportionate property wealth, marriage negotiation authority, and family labor allocation. Extraction is real but not absolute—the ie system does provide security, collective resource management, and ritual coherence that all members experience as partly beneficial (hence Tangled Rope rather than pure Snare at heir level). The extractiveness reflects that ~60% of the constraint's function serves wealth/authority concentration rather than collective coordination. Suppression (0.72): High. Powerful cultural mechanisms (filial piety, shame codes, honor systems) combine with legal authority (patriarch's property control, civil law family register) and economic dependence (women have no independent income; younger sons depend on family for capital, connections, or entry into alternative occupations). Defection costs are severe and multi-dimensional. Theater ratio (0.68): Moderate-high. By the 1920s (Tanizaki's period), ie enforcement increasingly relies on ritual and performance—elaborate deference codes, ceremonial acknowledgment of hierarchy, performative family meals and gatherings—rather than direct legal coercion. State enforcement capacity has weakened while cultural transmission remains strong. The theater reflects Goodhart drift: the ie persists through formalized ritual partly because younger siblings and women have begun acquiring exit options (urban employment, education, military rank for talented younger sons), forcing the system to rely more heavily on performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence. The patriarch sees Rope (coordination of house resources). The eldest son sees Tangled Rope (benefits from inheritance + obligation burden). Younger sons and daughters see Snare (trapped with no alternatives). Daughters-in-law see Snare with compounded victim status (victimized as daughters + re-victimized as in-laws). The state sees Piton (legal code persists but actual enforcement increasingly theatrical). The analytical observer risks seeing Mountain (naturalizing patrilineal succession as inherent to family structure, when it is actually a contingent institutional arrangement optimized for property concentration in early Meiji modernization).
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarch: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Absolute beneficiary with near-total exit options (can dissolve house authority without legal penalty, though socially rare). Eldest son: Victim + beneficiary + constrained → d≈0.55, f(d)≈0.75. Inherits property but constrained by absolute obligation to perpetuate ie (cannot reform, dissolve, or significantly alter house rules). Younger sons: Victim + trapped → d≈0.92, f(d)≈1.38. No inheritance; constrained into military/priesthood/merchant roles; cannot exit family obligation bonds. Daughters: Victim + trapped → d≈0.95, f(d)≈1.42. No inheritance; marriage is obligation transfer; no spouse choice; maximum extraction with zero autonomy. Daughters-in-law: Victim + trapped → d≈0.93, f(d)≈1.40. Enters new house with no autonomy; subject to mother-in-law authority; trapped within marital ie until widowhood. State: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Benefits from centralized family-unit taxation and conscription; maintains ie through legal apparatus; low enforcement cost because cultural theater carries enforcement burden.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING FAMILY SUCCESSION MANDATROPHY: The ie system illustrates why Tangled Rope is structurally distinct from both pure Rope and pure Snare. The patriarchal (institutional/arbitrage) perspective correctly identifies the system's coordination function—ie rules do enable property consolidation, transgenerational wealth accumulation, and stable authority structure. But this coordination function comes packaged with severe asymmetric extraction from women and younger sons. The system cannot dissolve the coordination without losing the extraction; conversely, it cannot maintain the extraction without preserving the coordination facade. This inseparability is the defining feature of Tangled Rope. Pure Rope would mean all perspectives see primarily coordination; pure Snare would mean no genuine coordination function, only extracted value. The ie system exhibits both. The mandatrophy is resolved by recognizing that the system's legitimacy (from beneficiary perspective) is constitutively dependent on its extraction (from victim perspective)—the two are locked together. This lock is precisely what makes it Tangled Rope rather than a hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_obligation_vs_coercive_extraction,
    'Is the ie system''s enforcement primarily cultural (shame, honor, filial piety internalization) or structurally coercive (property law, economic dependence)?',
    'Analysis of defection rates among Meiji-Taisho cohorts; comparison of ie compliance in communities with strong cultural transmission vs those experiencing modernization/urbanization; examination of legal sanctions vs social sanctions for ie violations',
    'If cultural dominance: classification softens toward Piton (theater mechanism is the primary enforcement). If coercive dominance: classification hardens toward Snare (structural extraction without internalization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_obligation_vs_coercive_extraction, empirical, 'Whether ie enforcement relies on cultural obligation or structural coercion').

omega_variable(
    generational_perpetuation_mechanism,
    'How does the ie system reproduce victimization across generations? Do daughters-in-law actively enforce the same constraints they experienced, or does enforcement rely on external male authority?',
    'Ethnographic/historical evidence on mother-in-law enforcement; analysis of female agency in ie perpetuation; comparison of constraint severity before/after women enter new houses; documentation of female resistance patterns (Tanizaki''s ''Atsumono'' provides narrative evidence)',
    'If women enforce: suggests symbiotic extraction mechanism where victims become enforcers (heightens Tangled Rope classification). If male authority dominates: suggests pure patriarchal extraction (Snare remains primary). If resistance emerges: suggests Scaffold perspective with sunset trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_perpetuation_mechanism, empirical, 'How ie victimization propagates across generations').

omega_variable(
    exit_option_emergence_timeline,
    'At what point (1890s, 1920s, 1945+) did modernization create genuine exit options for younger sons, daughters, and daughters-in-law that made the ie system''s constraints truly escapable rather than merely bearable?',
    'Historical markers: urbanization rates, education access, labor market development, female employment, legal reform (1947 Constitution), cultural shifts in marriage autonomy; correlation between exit option emergence and ie system actual (not legal) authority decline',
    'If exit emerges early (1890s-1910s): ie transitions to Scaffold (sunset already underway). If exit emerges late (1945+): ie remains Snare/Tangled Rope through Taisho period. Resolves whether Tanizaki''s ''Atsumono'' (1920s) captures system at peak extraction or already-declining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_emergence_timeline, empirical, 'Timeline of exit option emergence from ie constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_succession_and_decadence, 1868, 1898).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(famsuc_tr_t0, family_succession_and_decadence, theater_ratio, 0, 0.55).
narrative_ontology:measurement(famsuc_tr_t15, family_succession_and_decadence, theater_ratio, 15, 0.65).
narrative_ontology:measurement(famsuc_tr_t30, family_succession_and_decadence, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(famsuc_be_t0, family_succession_and_decadence, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(famsuc_be_t15, family_succession_and_decadence, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(famsuc_be_t30, family_succession_and_decadence, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_succession_and_decadence, resource_allocation).
narrative_ontology:affects_constraint(family_succession_and_decadence, meiji_conscription_system).
narrative_ontology:affects_constraint(family_succession_and_decadence, japanese_bride_price_institution).
narrative_ontology:affects_constraint(family_succession_and_decadence, female_property_exclusion_doctrine).

% DUAL FORMULATION NOTE:
% The ie system decomposes into three structurally distinct constraints: (1) Property succession rules (ε≈0.48, Tangled Rope—pure wealth concentration with weak coordination rationale); (2) Marriage obligation transfer (ε≈0.62, Snare—no coordination benefit, pure extraction from daughters and daughters-in-law); (3) Male labor allocation (ε≈0.55, Tangled Rope—conscription system tied to house status, mixed coordination/extraction). This story models the unified ie system; downstream stories track specific mechanisms. All three are linked through the patriarchal authority structure and the state's legal codification of the ie in civil law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_succession_and_decadence, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
