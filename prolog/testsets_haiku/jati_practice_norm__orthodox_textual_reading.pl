% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Hierarchy: Orthodox Scriptural Varna Reading
 *   domain: social/religious/political
 *
 * SUMMARY:
 *   This constraint instantiates the ORTHODOX TEXTUAL READING of the
 *   jati/varna kernel: jati boundaries are treated as fixed scriptural
 *   categories derived from the Vedic varna framework, where occupational
 *   role and ritual status are cosmologically ordained and inherent to birth.
 *   Deviation from one's assigned jati is defined as ritual pollution — a
 *   spiritual and social contaminant. Under this reading, the jati system is
 *   not a historical accident or a colonial imposition but a divinely
 *   authorized cosmic order (Vedic cosmology, śruti authority). The
 *   measurement interval spans roughly 200 years (late pre-colonial through
 *   post-independence India), during which this reading's authority has
 *   eroded in law and education but persists in ritual practice and
 *   brahminical discourse. The extractiveness is high and stable (0.79–0.82)
 *   because the framework consistently assigns polluting occupations to lower
 *   jatis with blocked mobility. Theater increases from t=0 to t=132 (peaking
 *   as reformist challenge intensifies and legal prohibitions take effect)
 *   then declines slightly as post-colonial reality settles — this reflects
 *   the constraint's shift from active enforcement through social/ritual
 *   power to a mix of cultural persistence and performative reassertion. This
 *   reading FORECLOSES the localized_practice_reading (which treats jati as
 *   fluid and locally negotiated) and COEXISTS_WITH the
 *   colonial_census_reading (which sees jati categories as reified by
 *   administrative apparatus rather than cosmological). The claim and metrics
 *   are independently authored: the constraint is CLAIMED as snare (high
 *   extraction, blocked mobility, asymmetric enforcement) and the metrics
 *   confirm it (high extractiveness, high suppression, theater rising then
 *   plateauing).
 *
 * KEY AGENTS:
 *   - brahmin_ritual_authorities: institutional power, civilizational horizon — set and interpret the scriptural framework, control ritual access, benefit from brahminical privilege
 *   - polluting_occupational_jatis: powerless, trapped in hereditary occupations, target of extraction
 *   - untouchable_jatis: powerless, face physical and shadow-pollution restrictions, maximum suppression
 *   - intermediate_jatis: moderate power, benefit from subordination of lower jatis but constrained by brahminical authority
 *   - brahmin families (non-ritual-specialists): powerful, inherit brahmin privilege even without ritual specialization
 *   - reform movements (absent): excluded from the orthodox reading's epistemic authority, argue for jati abolition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.81).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.78).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Hierarchy: Orthodox Scriptural Varna Reading").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'dcb63dcd-5547-489f-b9e4-18647a2cd07f').
narrative_ontology:cs_kernel_codification('dcb63dcd-5547-489f-b9e4-18647a2cd07f', fixed_text).
narrative_ontology:cs_authority_grounding('dcb63dcd-5547-489f-b9e4-18647a2cd07f', lineage).
narrative_ontology:cs_interpretation_layer_present('dcb63dcd-5547-489f-b9e4-18647a2cd07f').
narrative_ontology:cs_reading_relation('dcb63dcd-5547-489f-b9e4-18647a2cd07f', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('dcb63dcd-5547-489f-b9e4-18647a2cd07f', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('dcb63dcd-5547-489f-b9e4-18647a2cd07f', foundational, varna_categories_cosmologically_fixed).
narrative_ontology:cs_axiom_status(varna_categories_cosmologically_fixed, holdable).
narrative_ontology:cs_axiom_grounding('dcb63dcd-5547-489f-b9e4-18647a2cd07f', varna_categories_cosmologically_fixed, theological).
narrative_ontology:cs_axiom('dcb63dcd-5547-489f-b9e4-18647a2cd07f', foundational, ritual_pollution_inherent_to_birth).
narrative_ontology:cs_axiom_status(ritual_pollution_inherent_to_birth, holdable).
narrative_ontology:cs_axiom_grounding('dcb63dcd-5547-489f-b9e4-18647a2cd07f', ritual_pollution_inherent_to_birth, deontological).
narrative_ontology:cs_reference_frame('dcb63dcd-5547-489f-b9e4-18647a2cd07f', vedic_cosmic_order).
narrative_ontology:cs_drift_state('dcb63dcd-5547-489f-b9e4-18647a2cd07f', post_colonial_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dcb63dcd-5547-489f-b9e4-18647a2cd07f', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, polluting_occupational_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, untouchable_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, women_in_patrilineal_jatis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, intermediate_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_families_not_ritual_specialists).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, intermediate_jatis).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, vedic_cosmological_order).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, hereditary_occupational_assignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the scriptural varna framework as the divinely ordained cosmic order. They administer rituals that legitimize jati boundaries, control access to sacred knowledge and rites, and benefit from the hierarchical structure through monopoly on ritual authority, brahminical landholding, and exemption from pollution-associated occupations. Their identity is constituted as custodians of Vedic order.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Assigned hereditary occupations (leather work, sanitation, funeral rites, agriculture) declared ritually polluting by the varna framework. Legally and socially barred from: entry into other occupations, use of shared water sources, temple access, commensality with upper castes, intermarriage, and ritual participation in non-polluting domains. Exit by occupational change is blocked by caste enforcement mechanisms and by the doctrine that ritual pollution is inherent to birth, not chosen.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, polluting_occupational_jatis, payer,
    powerless, generational, trapped, regional).

% Assigned the lowest hereditary status in the framework — occupations involving death-handling and bodily waste. Subject to physical distance requirements, shadow-pollution concepts (even their shadow pollutes), ritual exclusion from all upper-caste spaces, and enforced economic dependency through monopoly assignment to the lowest-value tasks. The scriptural reading holds their status as cosmically fixed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, untouchable_jatis, payer,
    powerless, generational, trapped, regional).

% Land-owning cultivating castes and merchant castes holding middle positions in the varna hierarchy. They benefit from the subordination of polluting jatis below them and control labor arrangements, but remain subject to brahminical ritual authority and the framework's fixed hierarchical ordering. They cannot exit upward or redefine their ritual status through the scriptural reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, intermediate_jatis, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, intermediate_jatis, beneficiary).

% Inherit jati status through patrilineal descent but are additionally subject to gender-based ritual restrictions within that jati — menstrual seclusion, restricted temple access, purity codes around marital sexuality, and dependency on male kinship. The scriptural framework locates women's status below men of the same jati and binds their mobility through kinship and ritual obligations. Exit requires abandoning jati identity itself.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, women_in_patrilineal_jatis, payer,
    powerless, biographical, identity_locked, regional).

% Inherit brahmin status and its attendant privileges (exemption from pollution, access to sacred knowledge, landholding, trading, administrative roles) even if not ritual specialists. The framework grants them structural advantage independent of individual choice, though some may choose careers (learning, trade, warfare, administration) that nominally fall outside brahminical ritual. They benefit from the hierarchy without maintaining its daily machinery.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_families_not_ritual_specialists, beneficiary,
    powerful, civilizational, arbitrage, regional).

% Initially encountered the jati system as an ethnographic puzzle and collected textual readings (Vedic texts, dharmaśāstra commentaries) to understand it. Their classification efforts created administrative categories and census records that eventually reified jati into quantifiable units. They did not initially set the constraint's terms but their documentation and governance approach influenced how the constraint operated during and after colonial rule.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, colonial_administrators, observer,
    institutional, biographical, analytical, national).

% Emerged in the 19th–20th centuries arguing for jati abolition, caste-blind meritocracy, and reinterpretation of Hindu texts to permit mobility and marriage across jati boundaries. They are excluded from the orthodox scriptural reading's own framework because that reading treats jati boundaries as non-negotiable; reformers' arguments for reinterpretation are framed within the orthodox reading as heretical or Westernized. Their presence indicates the constraint is contested.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reform_and_nationalist_movements, excluded,
    organized, biographical, constrained, regional).

% The interpretive tradition of commentary on and elaboration of Vedic texts and dharmaśāstra that reproduces brahminical authority through learned discourse. Not an agent in the ordinary sense but a non-agent beneficiary — the constraint vindicates the tradition's epistemic claims, and the tradition's continued authority depends on the jati framework remaining fixed and authoritative.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_scholarly_tradition, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jati_practice_norm__orthodox_textual_reading, brahminical_scholarly_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, brahmin_ritual_authorities).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a labor-allocation problem by hereditary occupational assignment tied to cosmological order — ensures that polluting and low-status work is performed and socially devalued in a way that exempts ritual specialists and upper castes from contravening purity codes. The framework coordinates occupational inheritance with purity obligation.
% TRANSFER_FUNCTION: Moves economic surplus (crops, labor, ritual goods) from lower jatis to upper jatis and brahminical authorities, who collect ritual fees, land rent, labor obligation, and material gifts required by upper-caste ceremonies. It also transfers status and ritual authority exclusively to brahmin specialists, excluding lower jatis from sacred knowledge and ritual participation. The constraint channels material and symbolic goods upward through the hierarchy.
% ABSENT_VOICES: Polluting-jati practitioners who might argue for occupational choice; women in all jatis who might argue for gender parity within jati; reform movements (19th–20th century) that argued for jati abolition; lower castes' own interpretations of their history and status outside the brahminical framework. These voices are structurally excluded from within the orthodox scriptural reading because that reading treats them as outside-tradition or heretical. Their omission is built into the constraint's epistemic authority.
% DISAPPEARANCE_RATIONALE: If the orthodox scriptural varna framework ceased to have interpretive authority overnight, the jati system would not automatically disappear — localized practice readings and colonial census categories have created institutional momentum — but the justification for hereditary occupational assignment and ritual pollution concepts would collapse. Occupational mobility would become possible within a generation; ritual restrictions would lose their cosmological warrant and would erode through legal challenge and cultural renegotiation. The brahminical monopoly on ritual authority and textual interpretation would face legitimacy crisis. The system would reorganize around non-cosmological grounds (if it persisted at all).
% FOUNDING_PROBLEM: Early Vedic society needed division of labor and a cognitive framework to justify unequal distribution of ritual access, productive wealth, and occupational roles. The varna framework (originally four-part: brahmins/warriors/farmers/servants, with a fifth category of pollution-bearers added later) provided both organizational structure and cosmological legitimacy by placing the hierarchy within divine creation itself. This solved the problem of making hereditary inequality seem natural and unchangeable.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians and Vedic scholars dispute whether the varna framework in Rigvedic texts was originally a rigid hereditary system or a more fluid occupational distinction that became rigid later. However, outside the orthodox brahminical reading, the founding problem (how to justify hereditary inequality) is widely acknowledged as a HISTORICAL problem that the varna framework SOLVED, and a problem that no longer needs solving in modern nation-states with legal equality norms. The corroboration comes from: (1) historians and anthropologists documenting the shift from ritual-based to colonial-administrative to post-colonial legal organizing of caste; (2) reform-movement texts from the 19th century onward arguing explicitly that the founding problem is obsolete; (3) Indian constitutional law (1950 onward) explicitly abolishing caste-based discrimination, treating the problem as solved by legal fiat rather than through traditional hierarchy. Brahminical authorities attest the founding problem is still live (cosmological order is eternal), but this corroboration is internal to the reading itself — corroboration from outside (reformers, constitutional scholars, contemporary anthropologists) treats the problem as historically dead and the constraint as persistence by inertia.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and stable because the framework consistently assigns low-status, low-compensation occupations to lower jatis with legal and social mechanisms preventing exit. Suppression is high (0.78 at interval end) because compliance depends on enforcement through: ritual humiliation, caste panchayats (councils) that punish occupational transgression, denial of temple access and commensality, and internalized shame (identity-locked exit). Theater increases over the interval because: (1) early period (t=0–66) sees active ritual and social enforcement with strong cultural support; (2) middle period (t=99) experiences peak challenge from reform movements and nascent legal challenge (British law begins treating caste as superstition, Indian Constitution of 1950 abolishes caste-based discrimination); (3) brahminical authorities respond by emphasizing theatrical ritual reassertion (public yajnas/Vedic ceremonies, Sanskrit scholarship, Hindu nationalism) to defend the framework's authority; (4) late period (t=165–200) sees theater decline slightly as post-colonial Indian state enforcement mechanisms replace direct brahminical power, though cultural practice persists. The measurement grid tracks the constraint's transition from active social enforcement (high suppression, low theater) to increasingly performative maintenance (theater peaks then declines as post-colonial institutional backup settles in). Suppression requirement remains high throughout because the constraint is not self-enforcing — it requires continuous active mechanisms (caste councils, ritual gatekeeping, educational control) to persist against legal prohibition and reform pressure.
 *
 * PERSPECTIVAL GAP:
 *   Brahmin ritual authorities perceive this constraint as cosmic law — naturally emergent from divine order, not constructed — and experience it as coordination of labor aligned with dharma (duty). Polluting-jati practitioners experience it as enforced exclusion, with blocked exit justified by doctrines (ritual pollution, inherited karma) they did not author. The engine computes this divergence from structural data: brahminical authorities have high power and arbitrage-grade exit (can reinvent themselves as scholars, administrators, reformers while retaining status); lower jatis have zero power and trapped exit (birth into jati is permanent, occupational change is forbidden, geographical mobility is limited). The claimed type (snare) reflects the victim-seat experience; a brahmin authority might honestly perceive a rope (genuine coordination, mutual benefit through hierarchical order). The metrics (high extractiveness, high suppression, trapped exit) are the engine's measurement of this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin ritual authorities: d ≈ 0.1–0.2 (full beneficiary — they set the rules, collect ritual fees, inherit privilege, have highest power and best exit options). Polluting jatis: d ≈ 0.85–0.95 (full target — they pay through restricted occupations, low income, denied ritual participation, blocked mobility; trapped exit means no alternative available). Intermediate castes: d ≈ 0.5–0.6 (near-symmetric — they benefit from subordination of lower castes but pay through brahminical ritual authority and lack mobility within the fixed framework). Women in patrilineal jatis: d ≈ 0.75 (high target — inherit jati via patriline but additionally subordinated through gender restrictions, identity-locked exit through kinship obligation). The directionality derivation is driven by: power (structural authority to set rules), exit options (trapped → high d, arbitrage → low d), and beneficiary/victim declarations. No overrides needed; the structural data produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justifying hereditary inequality within cosmological order) was LIVE at the constraint's inception (roughly 1500 BCE in Vedic texts, crystallized in dharmaśāstra texts c. 200 BCE–400 CE). By the 19th century, that founding problem was DEAD: modern nation-states treat legal equality as foundational, and justifying hereditary inequality requires new arguments (genetic determinism, meritocratic hierarchy, etc.), not Vedic cosmology. The orthodox textual reading persists as a ZOMBIE CONSTRAINT — its mandate (cosmologically justify jati) has expired, but the institutional machinery (brahminical authority, ritual gatekeeping, caste councils) continues from inertia, cultural practice, and beneficiary entrenchment. This is NOT a snare that will be dismantled by persuasion (the beneficiaries are not ignorant; they have reformed their own arguments to persist without the full mandate). It IS a constraint whose decomposition is available but suppressed: the localized_practice_reading and colonial_census_reading offer alternative legitimations that make the constraint negotiable rather than cosmic. The orthodox reading forecloses those alternatives through its claim to scriptural exclusivity. Mandatrophy is resolved not through the snare's own mechanisms but through legal prohibition (Indian Constitution 1950, caste-based discrimination declared illegal), educational access (literacy and exposure to reform literature), and state capacity (civil marriage, caste-blind welfare, affirmative action programs that bypass caste-gating). The constraint persists theatrically because: (1) ritual practice (cremation rites, marriage rituals, purity codes) is deeply embedded in family and community life and does not require constant enforcement; (2) brahminical authorities have adapted to defend it through scholarship and cultural nationalism rather than rigid scriptural literalism; (3) lower castes themselves have made strategic use of the framework (upward mobility claims within intermediate castes, Dalit assertion of dignity within caste identities rather than abolition alone). The theater rise-and-plateau pattern (peaking at t=99 when legal challenge is strongest) reflects this adaptive persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmological_vs_constructed,
    'Is the jati/varna framework a genuine cosmological truth (as the orthodox reading claims) or a constructed institutional hierarchy that uses cosmological language as legitimation?',
    'Comparative historical analysis: if jati boundaries are cosmologically fixed, they should be stable and identical across regions and centuries; if constructed, they should vary locally and change over time. Evidence: (1) Vedic texts show different varna conceptualizations across texts and periods; (2) dharmaśāstra texts acknowledge regional variation in jati rankings and occupations; (3) ethnographic and census data show jati proliferation and merger in pre-colonial and colonial periods. This demonstrates change over time and local variation incompatible with cosmological fixity.',
    'If resolved toward ''constructed'': the orthodox reading loses its claim to cosmological inevitability and becomes one institutional arrangement among others. Jati boundaries become negotiable rather than fixed. The extractiveness drops below 0.7 and the type reclassifies from snare toward tangled rope or piton (depending on whether active enforcement persists). If resolved toward ''cosmological'': the reading''s authority holds and extractiveness remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmological_vs_constructed, empirical, 'Whether jati categories are cosmologically ordained or institutionally constructed.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.78) primarily structural (enforced through external sanctions: caste councils, ritual exclusion, economic dependency) or internalized (polluting jatis accept their status as natural/earned karma)?',
    'Post-abolition trajectory analysis: if suppression is primarily structural, it should decline rapidly after legal abolition of caste discrimination (post-1950 India, 1956 onward). If internalized, it should persist long after legal change because the target continues to believe the doctrine. Evidence: (1) occupational mobility increased measurably post-1950 (reservation programs, educational access); (2) ritual pollution beliefs persisted in practice even among legally mobile groups; (3) contemporary surveys show persistent caste-based discrimination despite legal prohibition, indicating both residual internalization and structural persistence. The ratio of behavioral compliance to self-identification post-abolition reveals the split.',
    'If primarily structural: legal reform and alternative education are sufficient to dissolve the constraint. If significantly internalized: the constraint carries forward via cultural transmission even after external enforcement collapses. Internalization implies the constraint operates at the biographical and family level rather than institutional level, requiring generational cultural change. The measured suppression (0.78) might underestimate the constraint''s effective suppression if internalization is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized; separability of constraint from social practice after legal abolition.').

omega_variable(
    brahminical_unified_interest,
    'Do all brahmin-identified groups (ritual specialists, scholars, landholders, reformers, political leaders) have a unified interest in maintaining the orthodox varna reading?',
    'Historical and contemporary heterogeneity analysis: brahmin reformers (Ram Mohan Roy, Jyotiba Phule''s brahmin allies, contemporary dalit-affirming brahmin scholars) explicitly rejected the orthodox reading and advocated jati abolition or reinterpretation. Brahmin political leaders (e.g., B.R. Ambedkar, Constitution drafter) wrote caste prohibition into law while claiming brahmin identity. Brahmin scholars today are divided between those defending traditional varna hierarchy and those criticizing it as historical oppression.',
    'If brahmin interest is unified (beneficiary monolith): the constraint''s persistence depends on brahminical institutional control (ritual authority, educational gatekeeping). If brahmin interest is fractured: some brahmin-identified agents are working against the constraint, and the snare''s persistence depends more on lower-caste compliance and state acquiescence than on unified brahminical enforcement. A fractured beneficiary coalition is vulnerable to internal legitimacy challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(brahminical_unified_interest, empirical, 'Whether brahmin-identified groups have unified interest in maintaining varna hierarchy or are internally divided.').

omega_variable(
    reading_foreclose_vs_coexist,
    'Does the orthodox textual reading FORECLOSE the localized-practice reading (logically incompatible in any single framework), or do the two COEXIST (held by different parties simultaneously)?',
    'Textual and institutional analysis: if the readings foreclose each other, no authoritative brahminical interpreter can hold both simultaneously. If they coexist, brahminical authorities may use the orthodox reading in formal ritual contexts while acknowledging local variation in informal practice, and different brahmin authorities may hold different readings without contradiction.',
    'If foreclosure: the orthodox reading is fragile — any admission of local practice variation destabilizes it. If coexistence: the constraint is more robust because it accommodates variation while maintaining brahminical authority. Coexistence also means the localized reading is NOT a separate constraint competing for the same institutional space but a complement to the orthodox reading used strategically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclose_vs_coexist, conceptual, 'Logical and institutional relationship between orthodox textual and localized-practice readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(jati_tr_t0, observed).
narrative_ontology:measurement(jati_tr_t33, jati_practice_norm__orthodox_textual_reading, theater_ratio, 33, 0.28).
narrative_ontology:measurement_basis(jati_tr_t33, observed).
narrative_ontology:measurement(jati_tr_t66, jati_practice_norm__orthodox_textual_reading, theater_ratio, 66, 0.35).
narrative_ontology:measurement_basis(jati_tr_t66, observed).
narrative_ontology:measurement(jati_tr_t99, jati_practice_norm__orthodox_textual_reading, theater_ratio, 99, 0.42).
narrative_ontology:measurement_basis(jati_tr_t99, observed).
narrative_ontology:measurement(jati_tr_t132, jati_practice_norm__orthodox_textual_reading, theater_ratio, 132, 0.46).
narrative_ontology:measurement_basis(jati_tr_t132, observed).
narrative_ontology:measurement(jati_tr_t165, jati_practice_norm__orthodox_textual_reading, theater_ratio, 165, 0.44).
narrative_ontology:measurement_basis(jati_tr_t165, observed).
narrative_ontology:measurement(jati_tr_t200, jati_practice_norm__orthodox_textual_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement_basis(jati_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.79).
narrative_ontology:measurement_basis(jati_be_t0, observed).
narrative_ontology:measurement(jati_be_t33, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 33, 0.8).
narrative_ontology:measurement_basis(jati_be_t33, observed).
narrative_ontology:measurement(jati_be_t66, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 66, 0.81).
narrative_ontology:measurement_basis(jati_be_t66, observed).
narrative_ontology:measurement(jati_be_t99, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 99, 0.82).
narrative_ontology:measurement_basis(jati_be_t99, observed).
narrative_ontology:measurement(jati_be_t132, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 132, 0.82).
narrative_ontology:measurement_basis(jati_be_t132, observed).
narrative_ontology:measurement(jati_be_t165, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 165, 0.81).
narrative_ontology:measurement_basis(jati_be_t165, observed).
narrative_ontology:measurement(jati_be_t200, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 200, 0.81).
narrative_ontology:measurement_basis(jati_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(jati_su_t0, observed).
narrative_ontology:measurement(jati_su_t33, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 33, 0.74).
narrative_ontology:measurement_basis(jati_su_t33, observed).
narrative_ontology:measurement(jati_su_t66, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 66, 0.76).
narrative_ontology:measurement_basis(jati_su_t66, observed).
narrative_ontology:measurement(jati_su_t99, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 99, 0.78).
narrative_ontology:measurement_basis(jati_su_t99, observed).
narrative_ontology:measurement(jati_su_t132, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 132, 0.78).
narrative_ontology:measurement_basis(jati_su_t132, observed).
narrative_ontology:measurement(jati_su_t165, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 165, 0.77).
narrative_ontology:measurement_basis(jati_su_t165, observed).
narrative_ontology:measurement(jati_su_t200, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 200, 0.78).
narrative_ontology:measurement_basis(jati_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the jati/varna kernel. The three sibling readings (orthodox_textual_reading, localized_practice_reading, colonial_census_reading) decompose a single persisting institutional structure — occupational and ritual hierarchy in South Asia — into three structurally distinct constraints, each with different ε values, different beneficiary/victim structures, and different type classifications. The orthodox reading treats jati as cosmologically fixed (high extractiveness snare, victims locked in place); the localized reading treats jati as negotiable coordination (lower extractiveness); the colonial reading treats jati as administratively reified (different beneficiaries: colonial power and enumerated caste elites). They are not three measurements of one constraint but three constraints reading the same kernel. The network links represent not causal dependency but rather epistemic competition and institutional influence: if the orthodox reading loses legitimacy, the localized and colonial readings provide alternative framings. Each story declares its own sibling relationships via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
