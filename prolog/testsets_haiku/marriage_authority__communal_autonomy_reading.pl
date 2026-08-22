% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority via Communal Religious Tradition
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   In legal pluralist constitutions, marriage authority is fragmented across
 *   religious and secular jurisdictions. This constraint embodies the
 *   communal autonomy reading: marriage law is authored by religious
 *   institutional hierarchies (grounded in scriptural tradition and
 *   jurisprudential lineage) and enforced by state courts. The state does not
 *   impose a uniform civil code but rather recognizes and enforces the
 *   diverse personal law codes of registered religious communities. The
 *   reading presents this as a solution to the pluralism problem—respecting
 *   doctrinal diversity and community self-governance. However, the
 *   constraint structurally privileges the religious establishment (via
 *   authority delegation), locks dissenters into conformity or exit, and
 *   creates friction for cross-community relationships. The measurement
 *   series tracks the tension: extractiveness rises modestly as
 *   constitutional pressure from equality doctrines accumulates (from 0.42 to
 *   0.58 over 40 years), while theater_ratio plateaus mid-range (0.42 at
 *   endpoint), indicating that the legitimacy of the arrangement rests partly
 *   on performative appeals to autonomy and pluralism even as its coercive
 *   mechanisms strengthen.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.58).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority via Communal Religious Tradition").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, 'a8978e13-dc00-4c51-aec5-e2278197f685').
narrative_ontology:cs_kernel_codification('a8978e13-dc00-4c51-aec5-e2278197f685', fixed_text).
narrative_ontology:cs_authority_grounding('a8978e13-dc00-4c51-aec5-e2278197f685', lineage).
narrative_ontology:cs_interpretation_layer_present('a8978e13-dc00-4c51-aec5-e2278197f685').
narrative_ontology:cs_reading_relation('a8978e13-dc00-4c51-aec5-e2278197f685', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8978e13-dc00-4c51-aec5-e2278197f685', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('a8978e13-dc00-4c51-aec5-e2278197f685', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8978e13-dc00-4c51-aec5-e2278197f685', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('a8978e13-dc00-4c51-aec5-e2278197f685', foundational, religious_communities_author_marriage_norms).
narrative_ontology:cs_axiom_status(religious_communities_author_marriage_norms, holdable).
narrative_ontology:cs_axiom_grounding('a8978e13-dc00-4c51-aec5-e2278197f685', religious_communities_author_marriage_norms, deontological).
narrative_ontology:cs_axiom('a8978e13-dc00-4c51-aec5-e2278197f685', foundational, state_enforces_but_does_not_impose_family_law).
narrative_ontology:cs_axiom_status(state_enforces_but_does_not_impose_family_law, holdable).
narrative_ontology:cs_axiom_grounding('a8978e13-dc00-4c51-aec5-e2278197f685', state_enforces_but_does_not_impose_family_law, conventional).
narrative_ontology:cs_axiom('a8978e13-dc00-4c51-aec5-e2278197f685', secondary, doctrinal_integrity_requires_institutional_hierarchy).
narrative_ontology:cs_axiom_status(doctrinal_integrity_requires_institutional_hierarchy, overridden).
narrative_ontology:cs_axiom_grounding('a8978e13-dc00-4c51-aec5-e2278197f685', doctrinal_integrity_requires_institutional_hierarchy, empirically_contingent).
narrative_ontology:cs_reference_frame('a8978e13-dc00-4c51-aec5-e2278197f685', communal_religious_authority_over_marriage).
narrative_ontology:cs_drift_state('a8978e13-dc00-4c51-aec5-e2278197f685', contemporary_constitutional_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a8978e13-dc00-4c51-aec5-e2278197f685', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_majoritarian_faction).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, cross_community_marriage_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, state_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers marriage law within the tradition, setting norms for validity, dissolution, and succession. Justified as preserving communal religious integrity and theological coherence. Authority derives from lineage (scriptural interpretation, jurisprudential tradition, recognized hierarchy). Collects no direct rents but accumulates institutional legitimacy and control over life-cycle events; amendments to personal law traditionally require their explicit consent or active participation in legislative process.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Born into the religious community but dissent from its marriage norms (on grounds of gender equality, LGBTQ inclusion, secular values, or contested doctrinal interpretation). Exit options are costly: leaving the community means social rupture, loss of kinship recognition, severed economic ties, and identity displacement. Attempting to marry within the tradition on dissident terms courts invalidity or community exclusion. The constraint forces conformity or exit; the identity-lock means exit carries the cost of cultural death.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, national).

% Avoids the political cost of imposing a Uniform Civil Code by deferring marriage authority to communal traditions. Enforces personal law codes through courts and registries but does not author them. This delegation dissolves majoritarian conflict: religious minorities retain self-governance; the secular majority avoids appearing to impose uniformity; the state retains ultimate enforcement capacity without authorship of norms. In exchange, state courts occasionally harmonize via constitutional interpretation (gender equality, due process), creating pressure on the religious interpretive hierarchy but stopping short of legislative override.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_legislature, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_legislature, agenda_setter).

% The faction within the religious community whose interpretation of tradition aligns with the established legal doctrine and institutional hierarchy. Their marriages are presumptively valid; their family arrangements receive legal recognition without doctrinal contestation. They benefit from the stability of shared norms and the institutional machinery that enforces them.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, community_majoritarian_faction, beneficiary,
    powerful, generational, mobile, national).

% Individuals who seek to marry across communal or religious boundaries. Personal law fragmentation creates legal friction: a marriage valid in one community's code may be unrecognized in another's; divorce or succession rights diverge; children's status becomes unclear. They must navigate multiple legal regimes or migrate to secular marriage (where available), at cost to identity or community standing.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, cross_community_marriage_seekers, payer,
    moderate, biographical, constrained, national).

% Intellectuals within the tradition who advocate for reform (gender equality, inclusivity, doctrinal reinterpretation) but lack formal authority in the legal hierarchy. They publish, petition, litigate, and mobilize community opinion, but the authorization structure gives them no seat at the legislative-amendment table. Legislative reform requires the religious leadership's participation or state legislative override—both of which are politically costly and rare.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, dissenting_scholars_and_reformers, excluded,
    moderate, generational, constrained, national).

% Reviews personal law codes against constitutional guarantees (equality, due process, freedom of conscience). Can strike down norms it deems unconstitutional but typically does so narrowly, preserving the personal law system itself. Acts as a pressure valve: individual injustices are occasionally remedied; systemic reform is deflected back to the legislative-amendment process.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% ABSENT_VOICES: Intra-community dissenters (reformists, LGBTQ members, gender-equality advocates) would object that the constraint suppresses internal diversity and locks them into conformity. Secular citizens and cross-community marriage seekers would object to the friction and limitation of choice. Democratic majoritarian advocates would argue marriage law belongs to the legislature, not to religious hierarchies. These voices are excluded from the formal legislative-amendment process—they can litigate and petition, but amendments to personal law traditionally require religious leadership participation or consent, keeping them at the margin of the law-making process.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 endpoint) because the constraint privileges institutional religious leadership in defining norms while binding dissenters through identity-lock and creating friction for those who dissent or cross boundaries. However, it is not pure extraction because genuine coordination functions are present: communities do solve collective-action problems around marriage validity, succession, and doctrinal coherence. The tension between real coordination and structural privilege produces a rope that looks increasingly extractive to those locked in. Suppression is high (0.71) because the constraint's persistence depends on actively maintaining the authority hierarchy and resisting constitutional encroachment—dissenters must conform, exit, or litigate in courts that rarely grant relief. Theater is moderate (0.42): the legitimacy story emphasizes pluralism and autonomy, but the enforcement machinery increasingly defends institutional prerogatives against constitutional challenge rather than serving the pluralist function. The measurement series shows extractiveness and suppression rising slowly and flattening after year 25, suggesting institutional hardening—the constraint settles into a stable but contested equilibrium as constitutional pressure accumulates without translating into formal reform.
 *
 * PERSPECTIVAL GAP:
 *   The gap between seats is substantial. From the religious leadership's perspective, the constraint solves a genuine coordination problem (how to maintain doctrinal integrity and community autonomy in a pluralist state) and is sustained by shared commitment to tradition. From the dissenter's perspective, the same constraint is a mechanism for suppressing internal dissent and enforcing conformity under the guise of autonomy. From the state's perspective, it is a politically convenient delegation that avoids majoritarian conflict while retaining ultimate enforcement power. From the constitutional court's perspective, it is an evolving boundary—personal law autonomy is legitimate up to the point of violating fundamental rights, but that boundary is itself contested. These divergences are not measurement errors but structural facts: the constraint genuinely has different meaning and force depending on one's position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership has near-zero directionality on this constraint (full beneficiary): they author the rules and enforce them against resistance, with no material exit cost. Intra-community dissenters have near-maximal directionality (full target): they are identity-locked (exit means cultural death), so even modest suppression translates to high effective extraction. Cross-community marriage seekers sit in the middle-high range (0.65–0.75): they face friction but are not identity-locked—they can marry outside the system at some social cost. The state legislature sits near zero on the beneficiary side (negative directionality) from a formal standpoint: it avoids the political cost of imposing uniformity; but this framing obscures that it retains ultimate enforcement capacity and benefits from the delegation (avoids conflict). The constitutional court has analytical directionality (0.5) by role: it reviews but does not revise systematically. The measured directional asymmetry drives the computed type at each seat: from the religious leadership's seat this may appear as protective coordination; from the dissenter's seat the same structure appears as coercive confinement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not satisfy mandatrophy (founding problem status = contested, disappearance verdict = world_rearranges). However, there is an emerging mandatrophy signal: the founding problem (how to respect communal autonomy without majoritarian imposition) was live at the constitution's founding but has become partially dead. Constitutional guarantees now provide individual protections; gender-equality doctrine is entrenched; LGBTQ inclusion is advancing in secular jurisdictions. These developments mean the founding coordination problem (protecting minorities from majoritarian law) has been partially solved by constitutional individual rights—the personal law system now functions primarily to defend institutional prerogatives against those guarantees, not to solve the original pluralism problem. However, the constraint persists because the legislature lacks political will to impose a uniform code (majority fatigue with conflict; religious groups mobilized against change) and the state benefits from avoided authorship. The constraint has shifted from coordination (plurality of valid traditions) to extraction (institutional hierarchy + individual suppression) without formally acknowledging the shift. This is not yet full mandatrophy, but it is the trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_coercion_boundary,
    'Does the constraint respect genuine communal autonomy, or does it enforce institutional hierarchy under the guise of autonomy?',
    'Empirical test: survey intra-community dissenters on whether they experience the constraint as protective autonomy or coercive conformity. If majority report coercion despite affirming community membership, the autonomy framing is contested. Compare with cross-community groups (LGBTQ, reformists, women) who have exited the system: do they report coercion-driven exit or ideological choice? Post-exit trajectory analysis: if dissenters'' agency expands after exit, suppression was structural; if it contracts (internalized), suppression was partially internalized.',
    'If autonomy is genuine, the constraint is coordination-heavy and dissenters'' exit is ideological choice (lower d for dissenters, lower χ). If autonomy is rhetorical cover, the constraint is extraction-heavy and dissenters'' exit is coercion-driven (higher d, higher χ, reclassify toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_coercion_boundary, empirical, 'Whether the communal autonomy framing accurately describes the constraint or masks institutional hierarchy.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-lock binding dissenters structural (social ostracism, economic dependence, legal invalidity of non-conforming marriages) or internalized (dissenters believe they deserve treatment, have internalized the norm)?',
    'Longitudinal study of exit trajectories: if dissenters report persistent identity-shame or norm-internalization after exit from the community, suppression is partly internalized; if they report relief and agency restoration, suppression was primarily structural. Interview dissenter-advocates on whether their mobilization is possible within the community or requires formal exit.',
    'If structural: the constraint''s suppression is a property of the institutional machinery (law, property, kinship). If internalized: the suppression persists after exit, making the constraint more extractive than the structural measure alone suggests (effective suppression rises because it is carried by the target).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression mechanism in identity-locked dissenters.').

omega_variable(
    constitutional_pressure_foreclosure_vector,
    'Are constitutional equality and rights guarantees gradually foreclosing the communal autonomy reading (making it logically incoherent with the rights framework), or are they merely creating pressure that can be managed through doctrinal harmonization?',
    'Jurisprudential analysis: track whether constitutional courts are claiming that personal law pluralism is compatible with rights guarantees (harmonization trajectory) or incompatible in principle (foreclosure trajectory). Examine whether recent cases are expanding or narrowing the scope of personal law exception to constitutional review.',
    'If foreclosure is occurring, the communal autonomy reading faces structural delegitimation over time—the axiom that communities author family law becomes untenable once rights guarantees are constitutionalized. This would shift the constraint toward snare (coercion masquerading as tradition) or piton (ritual maintenance of a form whose function has died). If harmonization is sustainable, the reading can persist as a live alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_pressure_foreclosure_vector, conceptual, 'Whether constitutional rights guarantees are foreclosing or merely pressuring the communal autonomy reading.').

omega_variable(
    kernel_reading_contestation,
    'Which sibling reading of the marriage_authority kernel is actually dominant in practice—does communal autonomy, secularist uniformity, gender rights, federalist pluralism, or judicial harmonization guide actual marital legitimacy determinations?',
    'Case-law audit: analyze recent appellate decisions on marriage validity, dissolution, and succession across religious communities. Classify each decision by which reading it instantiates (e.g., ''gender rights reading applied when court struck down unequal divorce rules''). Aggregate: which reading governs the largest fraction of marital arrangements?',
    'If communal autonomy dominates practice, the constraint''s structural delta is accurately authored. If another reading dominates (e.g., gender rights via incremental constitutional review), the practical constraint may be a different reading (judicial harmonization or gender rights), and this story should be reframed or supplemented with a second story capturing the dominant actual pattern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which sibling reading of marriage_authority actually governs family law practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__communal_autonomy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__communal_autonomy_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__communal_autonomy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t5, marriage_authority__communal_autonomy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(marr_be_t15, marriage_authority__communal_autonomy_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(marr_be_t25, marriage_authority__communal_autonomy_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(marr_su_t5, marriage_authority__communal_autonomy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(marr_su_t15, marriage_authority__communal_autonomy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t25, marriage_authority__communal_autonomy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.14).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five structurally distinct constraints, each instantiating a different reading: communal_autonomy_reading (this story), secularist_reading (state-monopoly reading), gender_rights_reading (rights-based reform reading), federalist_millet_reading (deliberate pluralism-as-anti-tyranny reading), and judicial_harmonization_reading (incremental constitutional floors reading). Each reading has a distinct ε (communal autonomy: 0.58; secularist: higher, more extractive if interpreted as majoritarian imposition; gender rights: higher, exposing institutional hierarchy as coercive). The readings coexist in contemporary jurisprudence—different parties, judges, and jurisdictions invoke different readings in different cases. Do NOT merge readings into one story or average their ε values. Each is a separate constraint with its own beneficiary/victim structure and classification. This story (communal autonomy reading) presents marriage authority as legitimate communal self-governance; the gender rights reading presents the same institutional structure as patriarchal suppression; the secularist reading presents it as irrational exception to democratic law. These are not observational variations on one constraint—they are genuinely different constraints with different ε values, grounded in different premises about who should author family law and on what grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
