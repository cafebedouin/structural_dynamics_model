% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Abolitionist Rejection of Dharmasastra and Caste Hierarchy
 *   domain: religious law / textual interpretation / normative authority
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist_rejection reading of
 *   the dharmasastra_corpus kernel. It treats the Dharmasastra textual
 *   framework and the caste system it legitimates as a snare: a structure of
 *   pure extraction whose claims to coordinate social order and cosmic
 *   harmony are ideological cover. The reading forecloses both orthodox
 *   literalist and reformist contextual readings by denying any remaining
 *   textual legitimacy. Key agents include textual intermediaries who
 *   administer and benefit from the framework, dominant caste groups who
 *   collect surplus and status, and subordinated groupsâDalits, Shudras,
 *   and womenâwho bear the extraction. Anti-caste movements are
 *   structurally excluded from the interpretive tradition.
 *
 * KEY AGENTS:
 *   - Textual intermediaries (agenda_setter/beneficiary): Brahminical scholars and priests who enforce textual rules and derive authority from them.
 *   - Dominant caste groups (beneficiary): Collect social and economic surplus from caste hierarchy.
 *   - Dalit communities (payer): Bear the most severe exclusion and labor extraction.
 *   - Shudra laborers (payer): Subordinated service and agricultural labor.
 *   - Women within caste (payer): Subordinated through patrilineal control and purity policing.
 *   - Anti-caste movements (excluded): Reject the framework entirely; absent from textual authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.9).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Abolitionist Rejection of Dharmasastra and Caste Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious law / textual interpretation / normative authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '90eb96d3-13ac-4afb-8680-8d1896a9d058').
narrative_ontology:cs_kernel_codification('90eb96d3-13ac-4afb-8680-8d1896a9d058', fixed_text).
narrative_ontology:cs_authority_grounding('90eb96d3-13ac-4afb-8680-8d1896a9d058', extraction).
narrative_ontology:cs_interpretation_layer_present('90eb96d3-13ac-4afb-8680-8d1896a9d058').
narrative_ontology:cs_reading_relation('90eb96d3-13ac-4afb-8680-8d1896a9d058', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('90eb96d3-13ac-4afb-8680-8d1896a9d058', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('90eb96d3-13ac-4afb-8680-8d1896a9d058', foundational, textual_authority_wholly_illegitimate).
narrative_ontology:cs_axiom_status(textual_authority_wholly_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('90eb96d3-13ac-4afb-8680-8d1896a9d058', textual_authority_wholly_illegitimate, deontological).
narrative_ontology:cs_axiom('90eb96d3-13ac-4afb-8680-8d1896a9d058', foundational, caste_hierarchy_must_be_abolished).
narrative_ontology:cs_axiom_status(caste_hierarchy_must_be_abolished, holdable).
narrative_ontology:cs_axiom_grounding('90eb96d3-13ac-4afb-8680-8d1896a9d058', caste_hierarchy_must_be_abolished, deontological).
narrative_ontology:cs_reference_frame('90eb96d3-13ac-4afb-8680-8d1896a9d058', caste_based_textual_hierarchy).
narrative_ontology:cs_drift_state('90eb96d3-13ac-4afb-8680-8d1896a9d058', contemporary_post_independence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90eb96d3-13ac-4afb-8680-8d1896a9d058', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, textual_intermediaries).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_groups).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_within_caste).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Brahminical scholars, priests, and jurists who preserve, interpret, and enforce Dharmasastra texts. They derive social authority, economic patronage, and ritual privilege from their role as gatekeepers of textual legitimacy. Their identity and status are fused with the textual framework; abandoning it would dissolve their institutional function.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, textual_intermediaries, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, textual_intermediaries, beneficiary).

% Landholding and socially dominant caste collectivities whose economic security, social status, and marriage networks depend on the maintenance of caste boundaries. They receive surplus labor, deference, and exclusive access to public goods from the hierarchy, though formal renunciation of caste identity is socially costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_groups, beneficiary,
    powerful, generational, constrained, national).

% Communities historically classified as untouchable, assigned the most degrading labor, excluded from temples, wells, and public spaces, and subjected to violence for caste transgression. Legal abolition of untouchability has not eliminated social exclusion or economic precarity; exit from the caste category remains structurally blocked by violence and boycott.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_communities, payer,
    powerless, biographical, trapped, national).

% Agricultural and service laborers subordinated within the varna hierarchy, obligated to serve dominant castes and excluded from Vedic education and ritual authority. Their labor surplus is extracted through hereditary service relationships and debt bondage tied to caste status.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laborers, payer,
    powerless, biographical, trapped, national).

% Women subordinated through patrilineal marriage rules, endogamy enforcement, and textual prescriptions on conduct. Their autonomy is transferred to husbands and kin groups through Dharmasastra-derived family law; caste purity is policed through control of female sexuality, making exit inseparable from breaking kinship identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_within_caste, payer,
    powerless, biographical, identity_locked, national).

% Political and intellectual movements that reject Dharmasastra's authority entirely and demand annihilation of caste. They are structurally excluded from the interpretive tradition and its legitimacy; their presence in public discourse delegitimizes the textual framework but they hold no seat within its authority structure.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, anti_caste_movements, excluded,
    organized, generational, mobile, national).

narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to coordinate social order by assigning ranked roles, ritual duties, and occupational functions through a divinely ordained textual framework; in the abolitionist reading this purported coordination is indistinguishable from domination and serves no legitimate collective problem.
% TRANSFER_FUNCTION: Moves social status, economic surplus, labor, and bodily autonomy from subordinated castes and women to dominant caste groups and textual intermediaries; transfers political silence and exclusion from the oppressed to the beneficiaries of hierarchy.
% ABSENT_VOICES: Dalit theologians, anti-caste philosophers, and secular critics who reject the textual framework entirely rather than seeking reinterpretation; they are absent from the Brahminical interpretive tradition and its authority structure.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra framework and caste hierarchy disappeared, hereditary occupation and exclusion would collapse, dominant caste privilege would lose its primary legitimacy mechanism, marriage and labor markets would reorganize, and the ritual status of textual intermediaries would dissolve.
% FOUNDING_PROBLEM: Historically purported to solve social disorder by establishing a stable, hierarchically ordered society tied to cosmic law (rita/dharma), where each varna performs its prescribed function and ritual purity is maintained.
% FOUNDING_PROBLEM_CORROBORATION: Anti-caste movements (Ambedkarite, Periyarist) and secular historians attest from outside the beneficiary set that the founding social-order problem is either a myth or has been superseded by modern egalitarian frameworks; beneficiaries assert it is eternal.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the system persistently transfers labor, status, and autonomy from subordinated groups to dominant castes and textual intermediaries. Suppression is 0.9 because exit is barred by violence, social boycott, and identity-lock. Theater ratio rises from 0.2 to 0.7 over the interval: as formal legal enforcement weakens (post-independence constitutional abolition of caste discrimination), the constraint's persistence depends increasingly on performative maintenance of ritual boundaries, symbolic hierarchy, and theatrical assertions of textual authority. Accessibility collapse is high (0.85) because the framework presents itself as natural, cosmic, and total, making alternatives nearly unthinkable within the traditional worldview. Resistance is substantial (0.75) due to sustained anti-caste movements, though they remain structurally excluded from the interpretive apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The textual intermediary seat experiences the constraint as legitimate civilizational authority and source of status; Dalit communities experience it as a totalizing apparatus of exclusion, violence, and labor extraction. The engine computes these divergent seat classifications from the same structural data: the agenda_setter/beneficiary has identity-locked exit but collects rents, while the payer has trapped exit and bears costs. The national spatial scope amplifies effective extraction for payers because verification of alternative social forms is difficult across a large population.
 *
 * DIRECTIONALITY LOGIC:
 *   Textual intermediaries and dominant caste groups are declared beneficiaries, placing their structural directionality near the beneficiary pole (low d, subsidized by the constraint). Dalits, Shudras, and women are declared victims (role: payer), placing their directionality near the full-target pole (high d, amplified extraction). Anti-caste movements are excluded rather than coordinated; their exclusion is the boundary the enforcement machinery maintains. The asymmetry is reinforced by differential exit options: beneficiaries are constrained but not trapped, while payers are trapped or identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents mislabeling by rejecting the reformist contextual reading's claim that a separable coordination function (an ethical core of dharma) exists within the text. If the founding problemâmaintenance of social order through ranked rolesâwere still live and the text genuinely coordinated collective life, the constraint might read as a tangled rope. From the abolitionist seat, the founding problem is dead, superseded by egalitarian modernity, and the constraint persists as a snare. The reformist reading's coordination claim is the exact mandatrophied justification that the abolitionist reading identifies as cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_foreclosure_ambiguity,
    'Does the abolitionist claim that the textual framework must be wholly abandoned logically foreclose the reformist claim that an ethical core is separable, or do they function as strategic poles within anti-caste politics?',
    'Comparative political sociology of anti-caste coalitions: whether abolitionist and reformist organizations cooperate or treat each other as legitimizing the enemy framework.',
    'If coexistent, the kernel generates a political continuum rather than a binary rupture; if foreclosed, the abolitionist reading enforces a zero-tolerance boundary toward textual engagement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_foreclosure_ambiguity, conceptual, 'Whether abolitionist rejection forecloses reformist contextual salvage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is caste subordination maintained primarily by structural violence and economic exclusion, or by internalized identity-fusion where subordinated groups accept hierarchy as natural and legitimate?',
    'Post-exit trajectory studies: measuring persistence of caste identification and hierarchy-enforcing behavior among subordinated groups after geographic or economic exit from traditional settings.',
    'If internalized, effective suppression exceeds the structural measure and resistance strategies must target cognitive liberation; if purely structural, material redistribution and legal enforcement suffice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in caste hierarchy.').

omega_variable(
    colonial_codification_contamination,
    'To what extent does the modern caste system reflect British colonial codification and enumeration of Dharmasastra versus pre-colonial textual and social practice?',
    'Historical archival research comparing colonial census categories and legal codes with pre-colonial vernacular records and ethnographies.',
    'If primarily colonial, the extraction''s origin is repositioned to modern state formation, altering the beneficiary set and directionality assignment for colonial versus indigenous authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_codification_contamination, empirical, 'Colonial versus pre-colonial origin of caste as enforceable structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.3).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 40, 0.45).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.55).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 80, 0.65).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 100, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 80, 0.83).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, reformist_contextual).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel decomposes into three structurally distinct readings. Orthodox_literalist treats the text as eternal revealed truth (high authority, low extraction from its own seat). Reformist_contextual treats it as historically conditioned (moderate authority, separable coordination). Abolitionist_rejection treats it as wholly oppressive (zero authority, pure extraction). They share the referent kernel but have different Îµ values, different beneficiary structures, and mutually exclusive normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
