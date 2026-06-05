% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Marriage Commitment Legitimacy — Exogenous Override Reading (Federal Coercion)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The Manifesto of 1890 represents a foundational legitimacy crisis in The
 *   Church of Jesus Christ of Latter-day Saints. In this reading — the
 *   exogenous override reading — the Manifesto is not a prophetic revelation
 *   but a document of institutional capitulation to federal coercion. The
 *   theological doctrine of eternal plural marriage remains doctrinally
 *   intact but practice is suspended under duress. Members experience a
 *   structural gap: they are told the doctrine is true and eternal, yet the
 *   institution suspends its practice under federal pressure. This reading
 *   instantiates a pure extraction regime where the federal government
 *   benefits from the constraint (elimination of plural marriage as a threat
 *   to American legal nationalism) while the LDS membership bears the cost
 *   (cognitive dissonance, legitimacy crisis, doctrinal abandonment despite
 *   theological affirmation). The constraint's suppression is high because
 *   multiple mechanisms prevent members from exiting or challenging the
 *   arrangement: legal barriers (polygamy remains criminalized),
 *   institutional barriers (the Church enforces compliance as a condition of
 *   membership), and cognitive barriers (members who recognize the coercion
 *   must choose between acknowledging institutional dishonesty or reframing
 *   the Manifesto as divinely revealed). The extractiveness increases over
 *   the measurement interval (0.52 → 0.68) as the practice suspension becomes
 *   normalized and the doctrine becomes increasingly performative — the
 *   institution and federal government have succeeded in making the
 *   constraint appear inevitable and unchangeable. The suppression
 *   requirement remains stable and high (≈0.72) throughout because both
 *   federal law and institutional policy must continuously enforce the
 *   prohibition of plural marriage practice. Theater increases modestly (0.48
 *   → 0.58) as the doctrine-practice gap grows: the doctrine is affirmed but
 *   not lived, creating performative theological work to reconcile the
 *   contradiction.
 *
 * KEY AGENTS:
 *   - Federal Government: Institutional beneficiary (institutional/arbitrage) — gains elimination of plural marriage as institutional competitor and social threat, at minimal federal cost. Extractiveness flows from membership TO government via Church institutional compliance.
 *   - LDS Membership Base: Primary victim (powerless/trapped) — internalize contradiction between doctrine (eternal plural marriage is true) and practice (suspended indefinitely). Cannot exit (religious identity is constitutive and identity-locked to community). Bear cognitive dissonance and legitimacy crisis as extraction cost.
 *   - Church Institutional Leadership: Intermediate position (institutional/constrained) — experience mixed extraction and coordination. Coerced to suspend practice, but deploy institutional authority to enforce the constraint on membership. Have constrained exit (defiance possible but at cost of legal status, resources, statehood). Manage the doctrine-practice gap through theological performance.
 *   - Fundamentalist Splinter Groups: Organized resistance (organized/constrained) — reject the Manifesto and attempt to continue plural marriage practice. Have organized power but face legal vulnerability, resource loss, and social marginalization. Resist the constraint through alternative institutional formation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.68).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Marriage Commitment Legitimacy — Exogenous Override Reading (Federal Coercion)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'e1ef2f24-6bf6-40da-9a03-2050dc137afc').
narrative_ontology:cs_kernel_codification('e1ef2f24-6bf6-40da-9a03-2050dc137afc', fixed_text).
narrative_ontology:cs_authority_grounding('e1ef2f24-6bf6-40da-9a03-2050dc137afc', extraction).
narrative_ontology:cs_reading_relation('e1ef2f24-6bf6-40da-9a03-2050dc137afc', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('e1ef2f24-6bf6-40da-9a03-2050dc137afc', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('e1ef2f24-6bf6-40da-9a03-2050dc137afc', foundational, manifesto_coercive_capitulation).
narrative_ontology:cs_axiom_status(manifesto_coercive_capitulation, holdable).
narrative_ontology:cs_axiom_grounding('e1ef2f24-6bf6-40da-9a03-2050dc137afc', manifesto_coercive_capitulation, empirically_contingent).
narrative_ontology:cs_axiom('e1ef2f24-6bf6-40da-9a03-2050dc137afc', foundational, doctrine_practice_gap_reveals_illegitimacy).
narrative_ontology:cs_axiom_status(doctrine_practice_gap_reveals_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e1ef2f24-6bf6-40da-9a03-2050dc137afc', doctrine_practice_gap_reveals_illegitimacy, deontological).
narrative_ontology:cs_reference_frame('e1ef2f24-6bf6-40da-9a03-2050dc137afc', prophetic_institutional_autonomy).
narrative_ontology:cs_drift_state('e1ef2f24-6bf6-40da-9a03-2050dc137afc', post_manifesto_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1ef2f24-6bf6-40da-9a03-2050dc137afc', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The membership faces maximum extraction: their theological understanding of eternal marriage is suspended by federal coercion without doctrinal revision. They cannot exit (religious identity is constitutive) and must endure the gap between doctrine and practice. Effective extraction is high (χ ≈ 0.78) — they bear the legitimacy cost while the institution extracts institutional survival through compliance.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Children born or sealed after 1890 inherit the practice suspension as their baseline. The extraction operates through socialization: they experience suspended plural marriage as normative without the doctrinal dissonance their parents felt. Suppression remains high (legal, social, institutional barriers to practice) but perceived extraction may be lower due to cognitive framing. The constraint persists through normalization.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Church leadership experiences mixed extraction and coordination. The constraint is enforced (suppression ≥ 0.60) and requires active institutional deployment. But the Manifesto also solves a genuine coordination problem: how to preserve institutional legitimacy and legal status while managing theological commitments. Leadership has constrained exit (they could defy federal pressure but would lose institutional resources and legal standing). They experience this as a hybrid: coerced coordination to manage survival. Theater is moderate (0.58) — the Manifesto is partly performative administrative document, partly genuine institutional response.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Splinter groups that reject the Manifesto (fundamentalist Latter-Day Saints) have organized resistance but constrained exit from the broader institutional context. They preserve doctrine by defying the constraint, but at cost of legal vulnerability, resource loss, and social marginalization. They experience the constraint as a Tangled Rope from organized perspective: genuine coordination problem (state pluralism vs institutional survival) but structured through extraction (federal coercion).
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The federal government benefits from the constraint at low cost to itself. The constraint solves the government's problem (controlling plural marriage practice) while the institution absorbs the legitimacy cost. The government experiences this as pure coordination: legal suppression of plural marriage, institutional compliance achieved. Effective extraction flows FROM the membership TO the government via institutional intermediation. The government has arbitrage options (could use direct prosecution, military occupation, continued statehood denial) and chose the path with lowest federal cost.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational view, the theological claim about eternal plural marriage has become largely performative. The doctrine is maintained (not revoked) but functionally suspended. It persists through institutional inertia and theological steadfastness rather than through active coordination or genuine extraction. Theater is high (doctrine is affirmed in principle but not practiced) — the piton classification reflects the degradation of the doctrine's functional role.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal analytical perspective, the exogenous override reading naturalizes the constraint as an inevitable collision between incompatible institutional systems: a theocratic religious framework encounters a nation-state legal monopoly. The exogenous override reading might naturalize this as an immutable institutional law — when institutions with competing authority claims collide, the more powerful institution's preferences override the weaker. However, this naturalizes what is actually a contingent historical configuration (American legal nationalism, Mormon institutional autonomy, specific moment of political settlement). The engine will flag this as a false summit candidate.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. This reflects the increasing normalization of the practice suspension despite doctrinal affirmation — the constraint has succeeded in making coercive accommodation appear inevitable. At t=0 (1890), extractiveness is lower (0.52) because the coercion is visible and transparent (federal pressure is explicit). By t=10, extractiveness increases (0.68) because the constraint has become institutionalized and naturalized — members born into the suspension experience it as baseline rather than coercion. The federal government's extraction is consistent throughout: they solved their problem (eliminating plural marriage as a competitor institutional force) with minimal cost to federal apparatus. The beneficiary (federal government) maintains its extraction through both legal suppression and institutional intermediation (Church enforcement of membership compliance). Suppression (0.72): High and stable. Multiple reinforcing mechanisms maintain the constraint: federal law (polygamy is criminalized nationwide), Church institutional policy (membership requires renunciation of plural marriage), social stigma (plural marriage practitioners are legally vulnerable and socially marginalized), and cognitive mechanisms (members internalize that the practice is impermissible). Suppression is not declining because both federal and institutional enforcement mechanisms remain fully active. Theater (0.58): Moderate and rising. The constraint includes performative elements: the doctrine is theologically affirmed but practically inert; leadership affirms eternal plural marriage in principle while enforcing its suspension in practice; the Manifesto itself is framed as prophetic revelation despite transparent external coercion. The theater increases over time as the doctrine-practice gap widens and requires greater interpretive effort to reconcile. The gap between doctrine (true and eternal) and practice (permanently suspended) creates contradictions that members must manage through cognitive and theological work.
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading produces sharp perspectival divergence from sibling readings. Unlike the endogenous reinterpretation reading (which frames the Manifesto as prophetic revelation by divine command), the exogenous override reading frames it as institutional capitulation — the gap between doctrine and practice is a visible scar of federal coercion, not a unified prophetic movement. Unlike the hybrid pragmatic reading (which frames the Manifesto as strategic deployment of prophetic authority to manage crisis), the exogenous override reading denies that Church leadership had genuine agency in choosing the response — they were forced to choose coercive accommodation or institutional dissolution. This reading produces maximum perceived extraction for trapped members: they are required to affirm a doctrine (eternal plural marriage) while practicing its negation (permanent celibacy or monogamy-only marriage). The piton perspective (civilizational view of the doctrine as performative) is consistent with exogenous override reading: the doctrine persists through institutional inertia, not through genuine theological function. The analytical observer's mountain perspective (viewing the collision as an inevitable law of institutional conflict) is flagged as a false summit in this reading: the constraint is not an immutable natural law but a contingent historical configuration of American legal nationalism, Mormon institutional autonomy, and specific political settlement.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading frames the Manifesto as coercive federal extraction via institutional intermediation. The federal government occupies the beneficiary position (arbitrage exit): they achieve their objective (eliminating plural marriage as a legitimate institutional practice) while retaining flexibility to use or withdraw legal pressure as needed. The LDS membership occupies the victim position (trapped exit): religious identity is constitutive and identity-locked to the community; they cannot exit without abandoning self-concept. The Church leadership occupies an intermediate position (constrained exit): they can theoretically resist federal pressure but doing so would forfeit legal status, institutional resources, and statehood opportunity. The exogenous override reading emphasizes that the membership's loss of doctrinal practice is extracted toward federal benefit, not toward Church institutional survival. The doctrine is maintained not to serve the membership's spiritual understanding but to preserve the institution as a going concern under federal legal pressure. This directionality (beneficiary: federal government; victim: membership) produces high effective extractiveness when combined with the trapped exit options of the membership. The Church institution experiences constrained exit (they could defy but at high cost) and mixed structural position (coerced to extract from membership while maintaining institutional viability). The suppression is external (federal law, legal penalties) and internal (Church enforcement as a condition of membership standing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy — potential misclassification between coordination and extraction. The Manifesto could be read as pure coordination (solving the legitimate problem of institutional survival in the face of legal prohibition) or pure extraction (federal government extracting compliance at membership cost). The exogenous override reading resolves mandatrophy by declaring the federal government as primary beneficiary and the membership as primary victim: the constraint serves the federal government's interest (eliminating plural marriage as a competitor institutional force) more than it serves any genuine coordination function between membership and Church. The Church's 'coordination' function is actually institutional intermediation of federal extraction. Suppression is active enforcement (not passive coordination) — both federal law and Church institutional policy must continuously enforce the prohibition. The doctrine-practice gap reveals the extraction: if this were genuine coordination, the doctrine would be revised to match the practice. Instead, the doctrine is maintained despite practice suspension, creating cognitive burden for members. The theater (0.58) indicates that the Manifesto includes performative elements: framing federal coercion as prophetic revelation, maintaining doctrine while suspending practice. This performative element is the signature of extraction-masquerading-as-coordination. The snare classification is affirmed: effective extraction (χ) remains high because membership has no exit options; they are trapped by religious identity and institutional belonging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_versus_practice_gap_irresolvability,
    'Can the doctrine of eternal plural marriage remain theologically intact while practice is permanently suspended, or does sustained practice suspension constitute de facto doctrinal abandonment?',
    'Historical analysis of theological elaboration after 1890: does the Church develop theology compatible with practice suspension, or do subsequent teachings attempt to reconcile the gap? Comparison with other doctrine suspensions (e.g., institutional racism, gender restrictions) and their eventual doctrinal revision.',
    'If gap is irresolvable: Exogenous Override reading reveals a legitimacy crisis (members recognize the contradiction) — extractiveness remains high because the contradiction persists. If reconcilable: the sibling readings (endogenous reinterpretation, hybrid pragmatic) become more structurally plausible — doctrine can accommodate the practice shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_versus_practice_gap_irresolvability, conceptual, 'Whether doctrine-practice gap can remain indefinitely or constitutes de facto revision').

omega_variable(
    federal_coercion_versus_institutional_agency,
    'Was the Manifesto primarily responsive to federal coercion (exogenous override reading) or did Church leadership retain genuine doctrinal and strategic agency in choosing the form of response (hybrid pragmatic/endogenous reinterpretation readings)?',
    'Historical examination of leadership communications (journals, private correspondence, council minutes) regarding federal pressure vs. theological reasoning. Counterfactual: what would the Church have done absent federal pressure? Did leadership deliberately choose Manifesto form for strategic reasons beyond mere capitulation?',
    'Pure coercion narrative (exogenous override): extractiveness stays high, doctrine becomes fully inert, membership bears maximum legitimacy cost. Leadership agency narrative (hybrid/endogenous): extractiveness is lower, doctrine is actively redeployed for institutional purposes, membership is partly collaborative in the adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_versus_institutional_agency, empirical, 'Whether Manifesto was coerced capitulation or strategic institutional choice').

omega_variable(
    membership_perception_of_legitimacy_gap,
    'Did members perceive the doctrine-practice gap as evidence of federal coercion (supporting exogenous override reading) or as prophetic revelation and divine adaptation (supporting endogenous reinterpretation reading)?',
    'Analysis of member testimonies, oral histories, journal entries from 1890-1920: how did members frame the Manifesto? Did they experience cognitive dissonance (gap between doctrine and practice) or reframe the doctrine itself (accepting the Manifesto as divinely revealed)? Do different cohorts show different perception patterns (converts vs lifelong members, women vs men, leaders vs rank-and-file)?',
    'If members perceive coercion: extracted legitimacy is the cost — the constraint persists through suppression of cognitive dissonance. If members reframe doctrine: the endogenous reinterpretation reading becomes empirically dominant — the constraint operates through doctrinal adaptation, not exogenous force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(membership_perception_of_legitimacy_gap, empirical, 'Member perception of Manifesto as coercion vs divine adaptation').

omega_variable(
    alternative_institutional_paths_closed,
    'In 1890, did the Church have genuine alternative institutional paths available (defiance, compromise, gradual transition) or were those paths foreclosed by federal capacity and institutional vulnerability?',
    'Comparative historical analysis: what happened to other institutions facing state coercion (other religious groups, confederate institutions, indigenous sovereignty)? What were the actual federal enforcement mechanisms available? Did the Church''s institutional position (statehood denial, property seizure, prosecution threats, military presence) genuinely foreclose alternatives?',
    'If alternatives were foreclosed: exogenous override reading is structurally correct — the Church faced true coercive pressure. If alternatives existed: leadership choice becomes central — the Manifesto was one option among others, suggesting endogenous agency and hybrid pragmatism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institutional_paths_closed, empirical, 'Whether alternative institutional paths were available or foreclosed').

omega_variable(
    reading_identity_entanglement,
    'Is the exogenous override reading itself a reading that members adopt to preserve identity (a form of identity-locking to a narrative of victimization) or an objective characterization of structural power relations?',
    'Psychological and narrative analysis: does adoption of the exogenous override reading serve identity-preservation functions for members (justifying continued membership despite contradictions)? Are there systematic patterns in which cohorts and demographics adopt which reading (endogenous, exogenous, hybrid)?',
    'If the reading serves identity functions: even ''objective'' characterizations of coercion may be partly driven by identity-locking dynamics — the reading reinforces members'' self-understanding as a coerced institution. This does not make the characterization false, but it complicates the epistemology of the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_entanglement, conceptual, 'Whether exogenous override reading itself functions as identity-locking narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_eor_theater_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mcl_eor_theater_t5, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 5, 0.54).
narrative_ontology:measurement(mcl_eor_theater_t10, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mcl_eor_extract_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mcl_eor_extract_t5, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(mcl_eor_extract_t10, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mcl_eor_supp_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(mcl_eor_supp_t5, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(mcl_eor_supp_t10, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The kernel 'marriage_commitment_legitimacy' decomposes into three readings: exogenous_override_reading (federal coercion, high extractiveness), endogenous_reinterpretation_reading (prophetic revelation, lower extractiveness), and hybrid_pragmatic_reading (strategic institutional adaptation, moderate extractiveness). These are NOT alternative measurements of one constraint but three structurally distinct readings of a contested kernel. Each reading has different ε values, different beneficiary/victim structures, and different classification profiles. The exogenous override reading (this file) frames the Manifesto as federal extraction; the endogenous reinterpretation reading frames it as divine revelation; the hybrid pragmatic reading frames it as institutional agency managing external pressure. All three are live within Mormon theology and scholarship. This file instantiates one reading only, per the ε-invariance principle applied to kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
