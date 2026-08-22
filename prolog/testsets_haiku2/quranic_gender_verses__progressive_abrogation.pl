% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses via Progressive Abrogation Reading
 *   domain: religious/jurisprudential
 *
 * SUMMARY:
 *   The progressive-abrogation reading interprets the Qur'an as containing an
 *   internal trajectory from patriarchal-constrained equality (7th-century
 *   Arabia) toward full gender equality (later egalitarian verses like
 *   49:13). This reading claims later, explicitly egalitarian principles
 *   abrogate earlier gender-differentiated rules via the Islamic
 *   jurisprudential principle of naskh. The reading transfers substantive
 *   legal authority from classical jurists (who hold the verses are eternally
 *   binding and hierarchical) to contemporary interpreters and movements that
 *   read equality into the later verses. The high extractiveness (0.91)
 *   reflects the complete normative reversal: under this reading, women's
 *   legal standing transforms from subordinate to equal across inheritance,
 *   testimony, guardianship, and divorce. The reading is claimed as
 *   tangled_rope because it both coordinates (provides a path for believers
 *   to hold Islam + equality) and extracts (delegitimizes classical authority
 *   and imposes epistemic frames on communities whose identity rests on the
 *   literal reading). Theater ratio rises modestly (0.28→0.42) as the reading
 *   stabilizes: initial scholarly debate gives way to ceremonial endorsement
 *   in institutional contexts, where the reading's rhetorical function
 *   (enabling legal reform while preserving Islamic legitimacy) begins to
 *   outweigh its exegetical content.
 *
 * KEY AGENTS:
 *   - Progressive Qur'anic scholars: institutional authority, agenda-setter position, professionally constrained (cannot exit the reading without career cost once adopted)
 *   - Women seeking legal parity: beneficiaries, but powerless and identity-locked (family/community exit costs are prohibitive)
 *   - Traditional jurists: institutional payers, identity-locked (professional identity constituted by defense of literal reading), high resistance
 *   - Communities bound to literal reading: organized payers, identity-locked (entire social order is normatively grounded in these verses), epistemically violated by reinterpretation
 *   - Reform movements: beneficiaries, institutionally constrained but gaining ground
 *   - Secular nation-states: powerful beneficiaries, external validators of the reading (national equality law gains Islamic legitimacy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.91).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.91).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses via Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/jurisprudential").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'd51c7ab5-894e-4672-85d2-3731733eb429').
narrative_ontology:cs_kernel_codification('d51c7ab5-894e-4672-85d2-3731733eb429', fixed_text).
narrative_ontology:cs_authority_grounding('d51c7ab5-894e-4672-85d2-3731733eb429', extraction).
narrative_ontology:cs_interpretation_layer_present('d51c7ab5-894e-4672-85d2-3731733eb429').
narrative_ontology:cs_reading_relation('d51c7ab5-894e-4672-85d2-3731733eb429', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('d51c7ab5-894e-4672-85d2-3731733eb429', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('d51c7ab5-894e-4672-85d2-3731733eb429', foundational, naskh_applies_to_gender_ordinances).
narrative_ontology:cs_axiom_status(naskh_applies_to_gender_ordinances, holdable).
narrative_ontology:cs_axiom_grounding('d51c7ab5-894e-4672-85d2-3731733eb429', naskh_applies_to_gender_ordinances, empirically_contingent).
narrative_ontology:cs_axiom('d51c7ab5-894e-4672-85d2-3731733eb429', secondary, later_egalitarian_verses_override_earlier).
narrative_ontology:cs_axiom_status(later_egalitarian_verses_override_earlier, holdable).
narrative_ontology:cs_axiom_grounding('d51c7ab5-894e-4672-85d2-3731733eb429', later_egalitarian_verses_override_earlier, empirically_contingent).
narrative_ontology:cs_reference_frame('d51c7ab5-894e-4672-85d2-3731733eb429', classical_gender_hierarchy_as_mutable).
narrative_ontology:cs_drift_state('d51c7ab5-894e-4672-85d2-3731733eb429', contemporary_institutional_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d51c7ab5-894e-4672-85d2-3731733eb429', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, egalitarian_quranic_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_jurists).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, secular_nation_states).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reform_oriented_religious_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and theologians who author and defend the progressive-abrogation reading. They interpret Qur'an 49:13 (universal human dignity) and related late-revealed verses as abrogating earlier gender-differentiated rules via the principle of naskh. They set the interpretive agenda within academia, reform movements, and some institutional religious spaces, but face professional isolation and fatwa-condemnation from traditional authorities.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_quranic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Women whose legal claims (inheritance, testimony, guardianship, divorce) would gain parity with men under this reading's interpretation. The reading's adoption would transfer substantive rights currently withheld by traditional jurisprudence. Their exit from the constraint system (leaving religious communities that enforce literal readings) carries high identity and family costs.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity, beneficiary,
    powerless, biographical, constrained, global).

% Scholars trained in classical jurisprudence (Hanafi, Maliki, Shafi'i, Hanbali schools) whose interpretive authority and institutional standing depend on defending the literal, unchangeable status of verses 4:11, 4:34, 2:282 as timeless divine law. Adopting the progressive reading would require abandoning career-long scholarship, institutional affiliation, and professional identity. Their resistance to this reading is enforced by institutional incentives and community loyalty.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_jurists, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, traditional_jurists, observer).

% Religious communities and movements (conservative institutional Islam, particular national legal systems, cultural constituencies) whose self-understanding, family structure, legal code, and authority hierarchy are constituted by the literal reading of gender verses. Accepting the progressive reading would constitute epistemic violence—delegitimizing the foundational narrative they hold sacred and dissolving the normative basis of their social order.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading, payer,
    organized, generational, identity_locked, regional).

% States that have codified civil equality law may find the progressive reading useful as a pathway to legal harmonization with Islamic constituencies without formal secularization. The reading provides internal-to-Islam justification for equal-rights laws, reducing the cost of enforcement against religious objections.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_nation_states, beneficiary,
    powerful, biographical, mobile, national).

% Islamic feminism, progressive Islam movements, and reform organizations that advocate for gender equality within Islamic frameworks. The progressive-abrogation reading is their primary intellectual tool for reconciling their commitment to both Islam and gender parity. Adoption of this reading strengthens their institutional and ideological position.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reform_oriented_religious_movements, beneficiary,
    organized, generational, constrained, global).

% The formalized jurisprudential tradition (centuries of tafsir, fiqh schools, precedent) that the progressive reading challenges. Not an actor but a framework within which actors are positioned. Inclusion for narrative completeness as the background authority being contested.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, classical_islamic_authority_tradition, observer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quranic_gender_verses__progressive_abrogation, classical_islamic_authority_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, reform_oriented_religious_movements).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical pathway to reconcile scriptural authority with gender equality: interprets the Qur'an such that its own later verses override earlier ones, allowing believers to maintain scriptural fidelity while adopting legal and social equality. Coordinates the theological space so that 'Muslim' and 'feminist' are not contradictory categories.
% TRANSFER_FUNCTION: Moves interpretive authority from classical jurisprudential schools (which defend literal, unchanging gender differentiation) to contemporary scholars and movements that claim lineage to Qur'anic egalitarian principles. The reading transfers substantive legal rights (inheritance, testimony, guardianship) from men to women within Islamic legal frameworks.
% ABSENT_VOICES: Conservative and traditional Islamic scholars who are systematically excluded from or marginalized in the scholarly spaces where this reading gains institutional traction (academia, reform organizations, liberal-leaning religious institutions). Their lived authority within their own communities is rendered invisible or dismissed as 'cultural patriarchy' by proponents of the reading. Communities whose identity is constituted by the literal reading are treated as subjects of reform rather than as parties to the hermeneutical conversation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and classical literal interpretation were universally reinstated, women's legal status in Islamic jurisdictions would revert to the narrower rights classical jurisprudence prescribes (restricted inheritance, reduced testimony weight, male guardianship). Institutional arrangements (family law, inheritance, witness requirements) in countries that have adopted progressive readings or use them as legitimating cover for civil equality would lose their internal-Islamic justification and face renewed contestation. The authority structure that produced this reading would be delegitimized, and institutional religious leadership would realign to classical gatekeepers.
% FOUNDING_PROBLEM: Historical inequality in 7th-century Arabia was addressed by Qur'anic revelation that granted women specific rights (property, divorce initiation, inheritance shares) unprecedented in the surrounding culture. But these provisions were articulated in ways that retained patriarchal guardiansip in some domains. The founding problem is: how can believers hold that the Qur'an is complete revelation AND that its gender provisions fall short of equality?
% FOUNDING_PROBLEM_CORROBORATION: Proponents attest the founding problem is live: women remain unequal under classical readings in many jurisdictions, and the Qur'an itself contains universalist language (49:13, 9:71) that seems to supersede gender differentiation. Critics (traditional scholars, literalist communities) attest the founding problem is misframed: the Qur'an provides perfect guidance, gender differentiation is divinely ordained and not a 'problem,' and reading equality into it is epistemic imperialism. Independent scholarly corroboration: academic historians of Islamic law acknowledge both the historical progression within the Qur'an AND the consensus of classical jurisprudence on gender hierarchy; whether the latter is binding or abrogated is the reading-dependent contested point itself.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading produces very high extractiveness because it claims to overturn centuries of jurisprudential consensus—the entire classical apparatus that defined women's legal position is reread as provisional, incomplete, or abrogated. From the reading's seat, this is liberation (extraction reverses). From the literal-reading seat, it is delegitimization (extraction imposed). Suppression is high (0.78) because the reading's institutional adoption requires suppressing alternative interpretations within the spaces where it gains ground—academia, reform organizations, liberal-leaning religious institutions. This is structural suppression: the reading cannot coexist in the same hermeneutical space with classical literalism (naskh claims that earlier verses cease to bind). Theater ratio reflects the rising ceremonial function: as the reading becomes institutionalized (universities, reform movements, national law), its rhetorical role (legitimating reform to religious constituencies) increasingly dominates over exegetical content. The reading's persistence does not depend primarily on winning the exegetical argument (classical scholars remain unmoved), but on institutional entrenchment and the convergence of state interests, feminist movements, and reform constituencies.
 *
 * PERSPECTIVAL GAP:
 *   The gap between progressive scholars and traditional jurists should be dramatic. From the progressive seat, the reading is a coherent exegetical move (naskh is a legitimate jurisprudential principle; later universal verses override earlier specific ones). From the traditional seat, the reading commits a category error: naskh applies to legal rules, not to eternal divine ordinances about human nature and complementary roles; applying naskh to verses the Qur'an itself presents as foundational violates the integrity of revelation. The engine should compute these seats very differently: progressive scholars get low d (agenda-setters, institutional power, benefiting from the reading's adoption) while traditional jurists get high d (their authority is being extracted, their interpretive monopoly is being challenged). The measurement trajectory shows why: as the reading gains institutional ground (universities, international organizations, reform movements), traditional scholars face rising suppression (their interpretations are actively excluded from official spaces), not because of state force but because institutional gating selects for progressive readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Women and reform movements benefit from the reading's adoption (d near beneficiary end, 0.0-0.3). Progressive scholars as agenda-setters are positioned symmetrically: they benefit institutionally but face professional costs in conservative spaces (d near 0.4-0.5). Traditional jurists and literal-reading communities are targets: their interpretive authority is extracted, their worldview is delegitimized, their exit is identity-locked (d near 0.8-1.0). The reading cannot be held as a neutral scholarly option—it is institutionally enforced in academic contexts and equally suppressed in traditionalist ones. The polarization is not incidental but structural to the constraint: naskh claims are all-or-nothing (a verse either is or is not abrogated), so coexistence with literal reading is impossible in the same hermeneutical space.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to reconcile revelation with equality—is genuinely alive for believers committed to both Islam and gender parity. The constraint is not zombified; women still lack full legal parity in most Islamic jurisdictions, and the reading actively addresses this. However, the reading's relationship to its founding problem is unstable: as it becomes institutionalized (national law, international bodies, university curricula), its function shifts from exegetical solution to ideological tool. The theater ratio's rise (0.28→0.42) and plateau suggest the reading is entering a phase where institutional adoption outpaces exegetical conversion—it is adopted in ceremonial contexts (legal reform, institutional legitimacy) more than in actual interpretive communities that have internally processed the argument. The mandatrophy risk is moderate: if secular institutions begin enforcing the reading as a form of cultural assimilation (obligating Islamic communities to adopt egalitarian readings as a condition of civic participation), the reading becomes extraction on top of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_applicability_boundary,
    'Can naskh (the jurisprudential principle of verse abrogation) legitimately be applied to verses describing eternal divine ordinances about human nature and complementary roles, or only to specific legal rules meant to be temporary?',
    'Historical-critical analysis of how classical jurists used naskh, and comparative exegetical study of whether gender verses are formally structured the same way as other abrogated verses (e.g., verses on alcohol, direction of prayer).',
    'If naskh is deemed applicable, the progressive reading''s core move is valid and women''s legal parity follows. If not applicable, the reading commits a category error and classical hierarchy remains the binding interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_applicability_boundary, conceptual, 'Whether the principle of abrogation applies to gender verses or only to time-bound legal rules.').

omega_variable(
    identity_locked_exit_costs,
    'For scholars and communities bound to the literal reading, how much of the resistance to the progressive reading is structural constraint (career/family/community exit is genuinely prohibitive) versus normative commitment (they genuinely believe the reading is exegetically wrong)?',
    'Qualitative research tracking scholars who adopted the progressive reading and the costs they incurred; analysis of whether exit becomes easier when institutional costs are removed (e.g., in diaspora contexts with multiple religious authorities).',
    'High structural cost + low normative commitment = the reading is sustained by institutional exclusion more than by hermeneutical persuasion; suggests suppression is the operative enforcement mechanism. High normative commitment = the reading has genuinely won the intellectual argument even where it faces institutional resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_costs, empirical, 'Whether resistance to the reading is structural or value-based.').

omega_variable(
    epistemic_violence_and_legitimacy,
    'When the progressive reading is institutionalized (adopted in national law, universities, international organizations) in contexts where the literal reading is the community''s normative baseline, does the reading constitute epistemic violence that delegitimizes those communities, or does it constitute liberation that overrides illicit patriarchal constraints?',
    'This is fundamentally a values question—whether preserving the epistemic integrity of religious communities takes priority over gender equality, or vice versa. No empirical evidence resolves it. The question marks a key omega for any reading that proposes large-scale normative reversal.',
    'If epistemic violence is deemed primary concern, the reading''s institutional adoption should be constrained and alternative hermeneutical paths (contextual, negotiated) should be pursued. If gender equality is deemed primary, the reading''s expansion is justified regardless of epistemic costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_and_legitimacy, preference, 'Priority ranking between community epistemic integrity and gender equality.').

omega_variable(
    theater_ratio_stabilization,
    'Why does theater_ratio plateau at 0.42 after time_point 25? Is the reading stabilizing at a particular institutional saturation point, or is the plateau an artifact of measurement boundaries?',
    'Extend the measurement interval beyond 2040 to observe whether theater ratio continues to rise, plateaus indefinitely, or begins to decay as the reading becomes so institutionalized that performative aspects fade and it becomes routine.',
    'A plateaued theater ratio may indicate the reading has reached maximal institutional penetration without converting majority actual interpretive practice—suggesting piton-adjacent dynamics where the reading is maintained ceremonially rather than functionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_stabilization, empirical, 'Whether theater ratio plateau indicates saturation or measurement artifact.').

omega_variable(
    sibling_reading_coexistence,
    'Can the progressive-abrogation reading coexist indefinitely with the literal_hierarchical reading in the same global Islamic landscape, or does institutionalization of one necessarily foreclose the other?',
    'Historical observation: do jurisdictions / institutional spaces that adopt the progressive reading eventually suppress or marginalize the literal reading, or do both persist in separate communities? Examine whether hybrid positions emerge that blend abrogation claims with contextual reframing.',
    'If coexistence is structural (different communities, different institutions), the constraint remains a tangled rope without foreclosure. If institutional adoption drives suppression, the reading becomes a snare within institutions that adopt it—beneficiaries and payers remain polarized but one is systematically excluded from authoritative spaces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether the progressive and literal readings can coexist institutionally or whether one foreclosed the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t5, quranic_gender_verses__progressive_abrogation, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(qura_tr_t5, projected).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quranic_gender_verses__progressive_abrogation, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t25, quranic_gender_verses__progressive_abrogation, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(qura_tr_t25, observed).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(qura_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t5, quranic_gender_verses__progressive_abrogation, base_extractiveness, 5, 0.73).
narrative_ontology:measurement_basis(qura_be_t5, projected).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.79).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quranic_gender_verses__progressive_abrogation, base_extractiveness, 15, 0.84).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t25, quranic_gender_verses__progressive_abrogation, base_extractiveness, 25, 0.9).
narrative_ontology:measurement_basis(qura_be_t25, observed).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.91).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.91).
narrative_ontology:measurement_basis(qura_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t5, quranic_gender_verses__progressive_abrogation, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(qura_su_t5, projected).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quranic_gender_verses__progressive_abrogation, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t25, quranic_gender_verses__progressive_abrogation, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(qura_su_t25, observed).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(qura_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, islamic_family_law_enforcement).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, islamic_inheritance_asymmetry).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel decomposes into three reading-specific constraints: progressive_abrogation (this story), literal_hierarchical, and contextual_egalitarian. Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and extraction profiles. They share the same kernel text but interpret it incompatibly. Links to downstream constraints (family law, inheritance) show how different readings of the kernel propagate through applied jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
