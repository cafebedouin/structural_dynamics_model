% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Marriage as Sacramental Samskara under Hindu Dharmashastra Authority
 *   domain: religious/political/family law
 *
 * SUMMARY:
 *   Hindu marriage law as governed by dharmashastra texts and brahminical
 *   authority presents itself as sacramental ritual (samskara) that
 *   transforms the married woman into a member of her husband's lineage
 *   permanently and irrevocably. The constraint combines real coordination
 *   (patrilineal property transmission and family labor) with asymmetric
 *   extraction (female labor, fertility, and property control concentrated in
 *   husband's lineage). The sacramental framing renders women's role divinely
 *   ordained rather than negotiated, making the constraint resistant to
 *   contractual exit or renegotiation. This is ONE reading of the contested
 *   kernel of family law authority; it coexists with secular contractual,
 *   Christian canonical, Muslim Shariat, and Parsi readings, each
 *   instantiating different constraints with different victim/beneficiary
 *   structures and extraction profiles.
 *
 * KEY AGENTS:
 *   - Brahminical priesthood: Interprets texts, performs rituals, maintains authority over marriage formation; benefits from sacramental reading because it locks ritual-dependent authority into place
 *   - Patrilineal joint family: Consolidates property and female labor through sacramental indissolubility and caste endogamy; benefits from inability of women to exit or claim inheritance
 *   - Upper-caste lineages: Preserve ritual status through endogamy rules that restrict marriage alliance; benefit from supernatural sanction of caste boundaries
 *   - Lower-caste women: Subject to sacramental indissolubility, caste-restricted alliance, and ritual contamination doctrine; trapped in marriages without divorce option
 *   - Unmarried women: Pressure to marry via ritual incompleteness doctrine; treated as incomplete (ardhangini) until married
 *   - Women seeking dissolution: Prohibited from divorce by sacramental metaphysics; exit is ritual-supernatural impossibility, not merely legal blockage
 *   - Secular legal authority: Observes constraint from outside; post-1950 India permits divorce under civil law but sacramental reading persists in community practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Marriage as Sacramental Samskara under Hindu Dharmashastra Authority").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious/political/family law").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '1dc6cf19-db4d-4e8d-9e29-11fdad6520ab').
narrative_ontology:cs_kernel_codification('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', fixed_text).
narrative_ontology:cs_authority_grounding('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', lineage).
narrative_ontology:cs_interpretation_layer_present('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab').
narrative_ontology:cs_reading_relation('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_reading_relation('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', family_law_authority__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', foundational, marriage_as_metaphysical_sacrament).
narrative_ontology:cs_axiom_status(marriage_as_metaphysical_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', marriage_as_metaphysical_sacrament, theological).
narrative_ontology:cs_axiom('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', foundational, wife_identity_constituted_by_marriage_not_negotiated).
narrative_ontology:cs_axiom_status(wife_identity_constituted_by_marriage_not_negotiated, overridden).
narrative_ontology:cs_axiom_grounding('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', wife_identity_constituted_by_marriage_not_negotiated, deontological).
narrative_ontology:cs_axiom('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', secondary, caste_endogamy_ritual_necessity).
narrative_ontology:cs_axiom_status(caste_endogamy_ritual_necessity, overridden).
narrative_ontology:cs_axiom_grounding('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', caste_endogamy_ritual_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', vedic_patrilineal_samskara_order).
narrative_ontology:cs_drift_state('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', contemporary_post_1950_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1dc6cf19-db4d-4e8d-9e29-11fdad6520ab', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, upper_caste_lineages).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, unmarried_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, women_seeking_dissolution).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, dharmic_cosmic_order).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, ritual_efficacy_of_sacrament).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, patrilineal_continuity_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers dharmashastra; performs rituals that constitute marriage sacrament; maintains canonical texts; adjudicates disputes through brahminical councils. Their authority rests on claimed direct transmission from Vedic tradition and exclusive access to ritual knowledge. They benefit from marriage's sacramental character because it locks authority into ritual performance and textual interpretation.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_priesthood, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Consolidates property, labor, and lineage through controlled marriages. The sacramental frame and caste endogamy norms protect joint family property from division via female inheritance or inter-caste marriage. Husbands and male household heads benefit from wife's ritual status as samskara-participant (not autonomous contractor) and labor availability. Marriage indissolubility is structural to family cohesion.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family, beneficiary,
    powerful, generational, constrained, regional).

% Preserve caste purity and ritual status through endogamy rules embedded in marriage law. The sacramental reading grants supernatural sanction to caste boundaries that restrict marriage alliance and protect status hierarchies. Violation carries ritual contamination, not merely civil penalty.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, upper_caste_lineages, beneficiary,
    powerful, civilizational, constrained, regional).

% Subject to marriage as sacrament and caste endogamy that restricts alliance to status-equivalent men (typically scarce, often already married). Once married, divorce is prohibited by sacramental indissolubility and ritual contamination doctrine. Exit is not merely legal but ritual-supernatural — leaving marriage means ritual death and caste expulsion. Labor and sexual access become obligatory within the marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_women, payer,
    powerless, biographical, trapped, local).

% Social and economic pressure to marry is enforced through ritual, property law, and family authority. Unmarried status is treated as incompleteness (ardhangini — half-self); unmarried women have no legitimate household role and remain under father's authority indefinitely. The constraint forces entry into marriage as the only recognized adult female status.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, unmarried_women, payer,
    powerless, biographical, trapped, local).

% Sacramental indissolubility prohibits divorce. A woman in an abusive or abandoned marriage has no legitimate exit: her sacrament binds her for life regardless of her husband's conduct. Seeking dissolution invokes ritual contamination, caste expulsion, and family violence. The sacramental frame makes exit metaphysically impossible, not merely legally blocked.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, women_seeking_dissolution, payer,
    powerless, biographical, trapped, local).

% Adjudicate disputes in family law using dharmashastra texts and precedent. They maintain consistency between sacramental theology and practical enforcement. As the constraint's operating authority, they have power to interpret and adapt rules; but they are also structurally locked into defending sacramental indissolubility because their legitimacy rests on the reading itself.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_councils, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, brahminical_councils, observer).

% In independent India post-1950, secular state law coexists with personal law jurisdiction, permitting Hindu Code (1956) to permit divorce while dharmashastra councils maintain sacramental reading as authoritative within religious community practice. They observe the constraint from outside but do not enforce it; the sacramental reading operates parallel to civil law.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_legal_authority, observer,
    institutional, generational, analytical, national).

% Advocate for divorce rights, remarriage of widows, and female property inheritance based on contractual readings of marriage and Vedic reinterpretation. They are structurally excluded from brahminical councils and have limited standing in family dispute adjudication. Their proposals would dissolve the sacramental reading but they lack institutional power to enforce alternatives.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, women_reformers, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Marriage as sacramental samskara solves the problem of binding women and property into patrilineal joint families with ritual sanction: the sacrament constitutes the wife as a member of the husband's lineage, integrates her labor and fertility into joint-family economics, and anchors property transmission through male descent with supernatural legitimacy. The ritual efficacy justifies restrictions on female autonomy, property rights, and exit — the sacrament is not a contract one party negotiates but a cosmic transformation one undergoes.
% TRANSFER_FUNCTION: Moves female labor (household, sexual, reproductive), property rights (bride-price received by father, lack of widow inheritance), and fertility (children belong to husband's lineage) from a woman and her birth family to the husband's lineage. The transfer is presented as sacred exchange (kanyadan — gift of a virgin), not economic transaction; sacramental framing makes it appear as gift rather than extraction.
% ABSENT_VOICES: Lower-caste women, widow-remarriage advocates, and women who fled forced marriages are structurally excluded from family law councils and have no voice in interpreting dharmashastra. Secular legal reformers and inter-caste marriage advocates would challenge the sacramental reading but are not heard in brahminical councils. The constraint's persistence depends on excluding voices that would contest the reading itself.
% DISAPPEARANCE_RATIONALE: If sacramental indissolubility and caste endogamy enforcement vanished overnight, marriages would become dissolvable at will (as secular law permits), women's property rights would shift from joint-family-absorbed to individual inheritance, inter-caste marriage would cease to carry ritual contamination, and brahminical councils would lose authority over family formation. The joint family itself would weaken as caste boundaries and female labor obligation ceased to be supernatural necessities. The constraint's removal would allow property, lineage, and marital authority to reorganize around female autonomy and contractual choice.
% FOUNDING_PROBLEM: Early Vedic and classical Hindu society required ensuring patrilineal property transmission, controlling female sexuality to establish paternity certainty, and binding women to joint-family labor systems without access to independent property or divorce. Marriage as sacrament solved this by rendering the union metaphysically unbreakable and the wife's role divinely ordained, not negotiated.
% FOUNDING_PROBLEM_CORROBORATION: Dharmashastra scholars and contemporary brahminical councils attest the founding problem — ensuring lineage stability and family cohesion — remains live. Historians and social scientists outside the benefiting parties attest that the founding problem was a solution to patriarchal property transmission specific to classical agrarian joint-family structures; independent India's secular legal code, property reforms, and widow-remarriage laws have substantially solved the coordination problem without the sacramental constraint, rendering it functionally obsolete. The disagreement is not empirical (the texts say what they say) but sobre whether the founding coordination problem persists.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68–0.71) because the sacramental reading consolidates female labor, fertility, and property rights into the husband's lineage without female autonomy or contractual renegotiation. Suppression is higher (0.72–0.76) because the constraint's persistence depends on active enforcement of caste boundaries, ritual contamination doctrine, and family-level coercion — alternatives are not merely unavailable but metaphysically impossible (exit is ritual death). Theater_ratio is moderate (0.35–0.43) because the sacramental ritual function is genuine and performed, but the ratio is sustained by the growing gap between claimed coordination function (family stability, property transmission) and actual enforcement activity (preventing inter-caste marriage, blocking widow remarriage, enforcing female obedience). The measurement trajectory (extraction declining 0.71→0.63 through t=50, then stabilizing at 0.68 projected) reflects declining brahminical enforcement authority and secular legal alternatives, but the projection stabilizes rather than declining further, indicating the reading's institutional persistence despite functional obsolescence. Suppression declines similarly (0.76→0.65, then stabilizes at 0.72), suggesting enforcement capacity is eroding but the constraint remains structurally enforced. Theater rises (0.35→0.43, then 0.48) indicating growing performative maintenance as the coordination function weakens but the constraint persists by institutional inertia. All three metrics are authored at every time point on a single shared grid (70 points: 8 time points × 3 metrics).
 *
 * PERSPECTIVAL GAP:
 *   From the brahminical priesthood seat: the constraint is genuine coordination solving the timeless problem of binding women and property into stable patrilineal families with ritual sanction. The sacrament is efficacious; alternatives are not merely legally blocked but metaphysically incoherent. From the lower-caste woman's seat: the same structure is enforced extraction of labor and fertility, justified by ritual claims she cannot contest because she lacks standing in interpretive councils. From the secular legal authority's seat: the sacramental reading is obsolete community tradition coexisting with civil law, neither enforcing the other but competing for legitimacy. The engine should compute these divergences from the structural data — the brahminical council's identity_locked exit and institutional power yielding a different d (and different type perception) than the lower-caste woman's trapped exit and powerlessness.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical priesthood are near-beneficiary (d~0.1): they set and interpret the constraint, benefit from authority lock-in, have constrained exit (identity-locked into priestly role). Patrilineal joint family are beneficiary (d~0.15): they benefit from female labor and property consolidation, maintain the constraint voluntarily. Upper-caste lineages are beneficiary (d~0.2): they benefit from endogamy rules protecting status. Lower-caste women are full targets (d~0.95): they pay through labor, fertility, and exit prohibition; trapped by property law and caste enforcement with no arbitrage. Unmarried women are near-target (d~0.85): pressure to enter marriage is enforced through social and property mechanisms. Women seeking dissolution are full targets (d~1.0): the constraint's entire enforcement machinery opposes their exit. The directionality derivation from beneficiary/victim + exit + power should yield these divergences without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (patrilineal property transmission in agrarian joint-family society) is substantially solved by secular law and economic change (independent property, female inheritance, legal divorce). The constraint persists despite functional obsolescence, indicating mandatrophy. The measurement trajectory confirms this: extraction and suppression decline through the interval (institutional authority eroding) while theater rises (enforcement becomes increasingly performative). However, the projected values (0.68 extractiveness, 0.72 suppression, 0.48 theater through t=70) suggest stabilization rather than collapse, indicating the constraint is transitioning from enforced coordination to Piton status — maintained by institutional inertia (brahminical councils, family practice) rather than by active beneficiary capture or live coordination function. The classification should reflect this: Tangled Rope (authentic coordination function + asymmetric extraction + enforcement) is accurate for the historical state (t=0–40); but the intermediate measurements (t=40–50) show drift toward Piton (theater rising, extraction declining, enforcement becoming symbolic). The claim (tangled_rope) captures the constraint's historical structure; the metrics capture its contemporary state oscillating between Tangled Rope and Piton as enforcement erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_incommensurability,
    'Is the sacramental reading of marriage (cosmic ritual transformation) logically compatible with the secular contractual reading (agreement between autonomous individuals), or do they foreclose one another?',
    'Examine whether any authoritative Hindu jurisprudence has integrated both readings into a single framework, or whether integration attempts always subordinate one to the other. Check whether communities holding the sacramental reading permit divorce (contractual exit), or whether the readings remain segregated.',
    'If forecloses: the readings coexist by institutional segregation (brahminical councils vs. secular courts), not by theoretical reconciliation. If integrable: a Hindu reformist reading might emerge that preserves sacramental language while permitting contractual exit. The classification would shift from coexists_with to influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_incommensurability, conceptual, 'Whether the sacramental and contractual readings are logically incommensurable or integrable.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72–0.76) primarily structural (property law, caste enforcement, family authority) or internalized (women''s belief in ritual inevitability, identity fusion with wife role)?',
    'Post-exit trajectory: if women who flee sacramental marriage and enter secular legal dissolution report rapid shift in self-concept and life choices, suppression is primarily structural. If suppression persists even after secular exit (shame, alienation, identity loss), the internalization component is substantial.',
    'If primarily structural: enforcement machinery could be dismantled by secular legal reform alone (as the Hindu Code attempted). If internalized: dismantling enforcement leaves internalized suppression intact, requiring cultural work beyond legal change. The effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether sacramental suppression is structural (law/enforcement) or internalized (identity/belief).').

omega_variable(
    brahminical_authority_transmission,
    'How is brahminical interpretive authority transmitted and enforced? Is it maintained by genuine community recognition of ritual/textual expertise, or by social hierarchy and coercion of non-brahminical castes?',
    'Compare brahminical authority over marriage law in communities where secular education is universal and alternative legal frameworks compete, versus communities with restricted education access. Check whether non-brahminical castes voluntarily defer to brahminical interpretation or whether deference is enforced by property/caste sanctions.',
    'If maintained by genuine expertise recognition: brahminical authority is Rope-like coordination. If enforced by hierarchy and coercion: authority is Snare-like, and the sacramental reading is the cover story for caste domination. The classification would shift from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_authority_transmission, empirical, 'Whether brahminical authority rests on recognized expertise or caste-enforced hierarchy.').

omega_variable(
    reading_obsolescence_and_revival,
    'Is the sacramental reading of marriage functionally obsolete in contemporary India (secular law, divorce rights, inter-caste marriage legal), or is it being deliberately revived as part of a nationalist/conservative political project?',
    'Track brahminical council activity, religious organization advocacy, and legal challenges to secular family law over the interval. Rising activity and organized defense of sacramental reading against secular law indicates revival; declining activity indicates obsolescence. Check whether revival is organic (community demand) or top-down (political/organizational drive).',
    'If obsolescent: the constraint persists by inertia and internalized suppression (Piton path). If revived: it is being reactivated as political project, and extractiveness and suppression might rise (Snare path). The measurement trajectory would inform the diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_obsolescence_and_revival, empirical, 'Whether the sacramental reading is functionally obsolete or being deliberately revived.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(fami_tr_t10, observed).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fami_tr_t20, observed).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fami_tr_t30, observed).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement_basis(fami_tr_t40, observed).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(fami_tr_t50, observed).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(fami_tr_t60, projected).
narrative_ontology:measurement(fami_tr_t70, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 70, 0.48).
narrative_ontology:measurement_basis(fami_tr_t70, projected).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t10, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(fami_be_t10, observed).
narrative_ontology:measurement(fami_be_t20, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(fami_be_t20, observed).
narrative_ontology:measurement(fami_be_t30, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(fami_be_t30, observed).
narrative_ontology:measurement(fami_be_t40, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(fami_be_t40, observed).
narrative_ontology:measurement(fami_be_t50, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(fami_be_t50, observed).
narrative_ontology:measurement(fami_be_t60, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(fami_be_t60, projected).
narrative_ontology:measurement(fami_be_t70, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(fami_be_t70, projected).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t10, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(fami_su_t10, observed).
narrative_ontology:measurement(fami_su_t20, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(fami_su_t20, observed).
narrative_ontology:measurement(fami_su_t30, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(fami_su_t30, observed).
narrative_ontology:measurement(fami_su_t40, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(fami_su_t40, observed).
narrative_ontology:measurement(fami_su_t50, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement_basis(fami_su_t50, observed).
narrative_ontology:measurement(fami_su_t60, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(fami_su_t60, projected).
narrative_ontology:measurement(fami_su_t70, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 70, 0.72).
narrative_ontology:measurement_basis(fami_su_t70, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcement).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, joint_family_property_rules).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel. It shares the same kernel (what marriage is, who has authority) with four sibling readings: secular_contractual_reading, muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and enforcement mechanisms. The readings coexist in India's plural legal system (personal law jurisdiction permits each reading to operate in parallel); they also compete in public discourse, courts, and reform advocacy. The ε-invariance principle requires that different readings be separate constraints with separate JSON files — this file captures the sacramental reading only; the secular reading has its own constraint_id, own metrics, own stakeholders. The network links indicate structural influence: changes in secular law availability affect the sacramental reading's suppression requirements (more escape routes → higher enforcement cost); changes in brahminical authority affect the secular reading's legitimacy base (if brahminical authority erodes, secular law fills the void).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, powerless, 0.95).
constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
