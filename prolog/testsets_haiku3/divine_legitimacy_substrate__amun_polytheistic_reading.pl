% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Priestly Amun-Ra Polytheistic Legitimacy
 *   domain: religious_political_economy
 *
 * SUMMARY:
 *   Ancient Egypt's priestly class maintained a centralized cosmological
 *   authority by claiming that all gods rank beneath Amun-Ra and that only
 *   the priesthood could interpret divine will. This constraint unified a
 *   dispersed, multi-ethnic state under a single legitimacy narrative while
 *   concentrating wealth and interpretive power in priestly hands. The
 *   pharaoh required priestly validation to rule, but the priesthood required
 *   pharaonic military power to enforce their interpretations against
 *   dissidents. This reading (amun_polytheistic_reading) instantiates the
 *   stable, accommodating version of this constraint—diverse regional cults
 *   are permitted as long as they acknowledge the central hierarchy. It
 *   contrasts with monotheistic attempts (atenist_monotheistic_reading) to
 *   collapse the hierarchy and with grassroots syncretism
 *   (folk_syncretistic_reading) that pragmatically mixes deities outside
 *   formal channels. The amun_polytheistic_reading was the dominant
 *   institutional framework for most of the pharaonic period, but it was
 *   never uncontested.
 *
 * KEY AGENTS:
 *   - Established priesthood: institutional agenda-setter, maintains interpretive monopoly over Amun-Ra cosmology, collects tithes and land, validates pharaonic authority.
 *   - Pharaonic authority: institutional power holder, structurally dependent on priestly validation, constrained by conditions they cannot unilaterally override.
 *   - Temple economies: organized beneficiaries, accumulate wealth and administrative function justified by the polytheistic framework.
 *   - Regional cult centers: moderate-power beneficiaries, derive legitimacy from alignment with central hierarchy but retain regional autonomy.
 *   - Heterodox practitioners: powerless, identity-locked targets of suppression, represent live alternative readings of the divine substrate.
 *   - Folk practitioners and non-affiliated communities: powerless, trapped, excluded from interpretive authority but required to accept priestly readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Priestly Amun-Ra Polytheistic Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious_political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '2cfb315e-9ce9-4582-a78c-61bcfc26cef7').
narrative_ontology:cs_kernel_codification('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', distributed).
narrative_ontology:cs_authority_grounding('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', lineage).
narrative_ontology:cs_interpretation_layer_present('2cfb315e-9ce9-4582-a78c-61bcfc26cef7').
narrative_ontology:cs_reading_relation('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', foundational, polytheistic_hierarchy_legitimates_rule).
narrative_ontology:cs_axiom_status(polytheistic_hierarchy_legitimates_rule, holdable).
narrative_ontology:cs_axiom_grounding('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', polytheistic_hierarchy_legitimates_rule, conventional).
narrative_ontology:cs_axiom('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', foundational, priestly_interpretation_is_divinely_authorized).
narrative_ontology:cs_axiom_status(priestly_interpretation_is_divinely_authorized, holdable).
narrative_ontology:cs_axiom_grounding('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', priestly_interpretation_is_divinely_authorized, theological).
narrative_ontology:cs_reference_frame('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', established_priestly_cosmological_order).
narrative_ontology:cs_drift_state('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', late_new_kingdom_pharaonic_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2cfb315e-9ce9-4582-a78c-61bcfc26cef7', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, established_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, heterodox_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, non_affiliated_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretive apparatus of the polytheistic cosmology, particularly the reading of Amun-Ra as supreme among gods. Maintains ritual calendars, initiates pharaohs, validates decisions through oracle consultation, and administers temple properties. Holds interpretive monopoly: only they can render authoritative readings of divine will. Collects substantial wealth through temple donations, grain tithes, and land holdings. Their authority is self-reinforcing: they validate the pharaoh, and the pharaoh's power validates them.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, established_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).

% Must obtain priestly validation to be deemed legitimate ruler—coronation rites, divine endorsement through oracle, recognition as 'son of Amun-Ra.' This validation is structurally necessary but discretionary: the priesthood can withdraw it. In exchange, the pharaoh grants land, exemptions, and deference to priestly interpretations of divine will. The pharaoh also benefits from the legitimacy the priesthood provides, but cannot unilaterally redefine the god or bypass priestly mediation. Any attempt to rule without priestly approval provokes religious crisis.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority, beneficiary).

% Benefit from the established polytheistic framework by maintaining local temples to Amun and other recognized deities. They receive pilgrims, donations, and maintain community cohesion through recognized ritual practice. Their authority derives from alignment with the central interpretive framework—they cannot claim to represent gods outside the authorized cosmology without losing legitimacy and resources.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers, beneficiary,
    moderate, generational, constrained, regional).

% Temple institutions—as landholding, wealth-accumulating entities—prosper under the established polytheistic reading because the constraint directs persistent resources to temples as 'houses of the gods.' They employ scribes, craftspeople, laborers. They function as stores of value, lenders, and administrative centers. The Amun-Ra supremacy framing legitimizes their wealth accumulation and administrative function.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies, beneficiary,
    organized, generational, arbitrage, national).

% Any who propose alternative divine readings (reranking the gods, claiming direct revelation outside priestly channels, syncretizing with foreign deities) face marginalization or violent suppression. Their suppression is structural: the priesthood has the institutional apparatus and the pharaon's sanction to enforce orthodoxy. Heterodoxy is costly because it risks social exclusion, property loss, and physical punishment. The constraint's persistence depends on their suppression.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, heterodox_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Rural villages, enslaved persons, migrant laborers—those outside the formal ritual system—are required to accept priestly interpretations of divine will without direct access to validation channels. They bear costs (tithes, corvée labor for temple construction, conscription justified through 'divine will'). They cannot contest the interpretations; resistance carries severe penalties. Their exclusion from interpretive authority is structural and complete.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, non_affiliated_communities, payer,
    powerless, immediate, trapped, local).

% Potentially present during periods of monotheistic experimentation—advocates for exclusive worship of Aten, rejection of other gods, and direct pharaonic revelation (not priestly mediation). They are actively excluded from the current constraint by suppression and institutional design. They would argue for collapsed hierarchy (one god, one path), which would hollow out the priestly interpretive monopoly. Their exclusion is enforced.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_dissidents, excluded,
    moderate, biographical, trapped, national).

% Household and village ritual specialists who conduct their own rites, consultations, and interpretations of divine will independent of the priesthood. They are tolerated when they operate within recognized polytheistic bounds but face suppression if they claim authority equal to or independent from the established priesthood. Their autonomy is structurally limited.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, folk_practitioners, excluded,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, established_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological framework for a multi-ethnic, multi-regional polity: a shared understanding that all gods occupy ranked positions under Amun-Ra, that the pharaoh is legitimate only through priestly validation, and that the priesthood is the authorized interpreter of divine intent. This allows diverse regional cults and social groups to coexist under a common legitimacy narrative without requiring consensus on first principles.
% TRANSFER_FUNCTION: Moves wealth (land, grain tithes, precious goods, labor) from the broader population to temple institutions and the priesthood; moves authority from local practice to centralized priestly interpretation; moves political power from any single pharaoh to the priesthood's conditional endorsement (which they can withdraw). The pharaoh receives validated legitimacy in exchange, but at the cost of deference to priestly judgment.
% ABSENT_VOICES: Heterodox practitioners (those who rank gods differently, claim direct revelation, or propose monotheism) are structurally excluded from the conversation; their alternative readings are suppressed, not debated. Folk practitioners at the village level have no voice in cosmological interpretation. Enslaved persons and non-affiliated communities bear the costs but have no standing to contest the framework. Any who question the priesthood's interpretive monopoly are marginalized.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if priestly mediation ceased to be required for legitimacy and any community could interpret the gods independently—political authority would fragment immediately. Regional governors could claim independent divine sanction. Temple wealth would be redistributed or seized by emergent powers. The unifying cosmological narrative would collapse into competing local cosmologies. The pharaonic state would lose its primary mechanism for binding dispersed populations to a single legitimacy claim.
% FOUNDING_PROBLEM: Early dynastic Egypt required a way to bind together geographically dispersed regions with different local deities and traditions under a single authority. The polytheistic hierarchy—with Amun-Ra as supreme—provided a framework where local gods could retain significance while acknowledging central authority. The priesthood's interpretive monopoly solved the problem of who decides what the gods mean, preventing rival claimants from each asserting independent divine authority.
% FOUNDING_PROBLEM_CORROBORATION: The priesthood attests the founding problem is perpetually live—unification requires constant priestly interpretation to prevent chaos. Regional governors and emerging monotheistic movements (evidenced in late New Kingdom texts and Amarna Period experimentation) attest that the founding problem has shifted or been solved: regional identity is now strong enough to sustain without priestly cosmological validation, and alternative readings (including monotheism and folk practice) coexist with the official framework. Historical evidence (temple records, administrative papyri, later theological texts) from outside the priesthood supports the contested reading: the constraint persists by inertia and institutional power, not by solving an active coordination problem.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial wealth transfer to temples and priesthood, but remains below pure-extraction levels because the priesthood does provide genuine coordination function—they solve the problem of multiple deities and competing local authority claims. Suppression (0.58) is high enough to constitute active enforcement (heterodox practitioners are suppressed, folk practice is constrained) but not total: regional variation is tolerated within the hierarchy, and folk practitioners are not eliminated. Theater ratio (0.41) indicates that a significant share of priestly activity is performative maintenance of their monopoly (elaborate ritual, oracle consultation, theological elaboration) rather than strictly necessary for coordination. The constraint's persistence depends on continuous enforcement: if the priesthood ceased to validate pharaohs or if alternative readings gained institutional support, the system would fragment. Accessibility collapse (0.72) is high because once the polytheistic hierarchy becomes the accepted framework, alternatives are cognitively difficult to imagine—the cosmology appears natural and immovable. Resistance (0.48) is moderate: dissidents exist and periodically challenge the framework, but lack institutional power to succeed durably.
 *
 * PERSPECTIVAL GAP:
 *   The established priesthood claims this constraint solves an essential coordination problem: without a unified cosmological framework and a designated interpreter of divine will, Egypt would fragment into competing religious factions and civil conflict would ensue. From their seat, they are custodians of public order and cosmic harmony. From the seat of heterodox practitioners and non-affiliated communities, the constraint is coercive maintenance of a monopoly that benefits the priesthood and constrains alternative spiritual paths. From the pharaonic seat, the constraint is a necessary cost of rule (they must defer to priestly judgment to maintain legitimacy) but also a brake on their power (they cannot act without priestly approval, and the priesthood can withdraw support). The engine computes these divergent types from the structural data: priesthood ≈ rope, pharaoh ≈ tangled_rope, heterodox ≈ snare, non-affiliated ≈ snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The established priesthood has near-zero directionality (d ≈ 0.05, full beneficiary end): they set the rules, collect the rents, and face minimal costs. The pharaonic authority sits near d ≈ 0.50 (symmetric): they benefit from the legitimacy but are constrained by the priestly condition. Heterodox practitioners have high directionality (d ≈ 0.90, target end): they bear the costs of suppression, have trapped exit, and gain nothing from the constraint. Temple economies (d ≈ 0.10): beneficiaries collecting rents. Regional cults (d ≈ 0.35): beneficiaries with slightly constrained options. Non-affiliated communities (d ≈ 0.85): targets bearing tithes and corvée. The divergence in directionality between seats is the primary reason the engine should compute different types for different seats: the priesthood experiences this as coordination (rope-like) from their perspective; the heterodox practitioners experience it as pure extraction (snare-like) from theirs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is unified rule over a multi-ethnic, multi-religious state. This is genuinely solved by the polytheistic hierarchy in the early periods—regional cults retain identity while acknowledging central authority, reducing interstate conflict. However, by the later New Kingdom, the problem's character changes: regional identity is stronger, centralization becomes harder to maintain, and the constraint's function shifts from coordination to rent extraction and maintenance of institutional privilege. The theater ratio rises (0.28 → 0.42) as the share of priestly activity devoted to ritual elaboration and theological defense increases relative to functional conflict resolution. The measurement series shows rising extractiveness over the interval (0.48 → 0.62), suggesting the constraint is accumulating rents faster than it is solving coordination problems. This is a piton candidate (the founding problem is dead or solved, but the institutional apparatus persists through inertia and enforcement). The tau mismatch: founding_problem_status='contested' and disappearance_verdict='world_rearranges' signals that the system persists not because it solves an active problem but because dismantling it would trigger reorganization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_polytheistic_hierarchy,
    'Is the ranked polytheistic cosmology (with Amun-Ra at the apex) a discovered feature of Egyptian religious reality, or a constructed arrangement that benefits the priesthood by appearing natural?',
    'Comparative analysis of earlier Egyptian religious texts (Old Kingdom) vs. later theological elaborations (New Kingdom) showing when the Amun-Ra supremacy claim became formalized; archaeological evidence of how regional cults were subordinated to central temple authority; ethnographic parallels from other centralized states using ranked cosmologies.',
    'If discovered/natural: the constraint is a mountain-like feature of Egyptian civilization. If constructed: it is a false summit—the priesthood benefits from appearing to transmit eternal divine order while actually constructing and maintaining it. This reading would shift classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_polytheistic_hierarchy, conceptual, 'Whether polytheistic hierarchy is discovered or constructed religious reality.').

omega_variable(
    priestly_interpretation_discretion,
    'How much genuine discretion do priestly interpreters have in reading divine will, vs. how much are they bound by fixed ritual procedure and canonical texts?',
    'Analysis of priestly training texts, oracle consultation records, and variation in interpretations across temples and periods. If interpretation is rigidly procedural (oracles always respond within fixed patterns), priestly discretion is low and the mechanism is more mechanical. If interpretation varies substantially, discretion is high and the priesthood has true power to shape outcomes.',
    'High discretion = priesthood holds real power; the constraint captures significant extraction. Low discretion = the priesthood is itself constrained by procedure; the extraction is lower and the constraint may be better described as coordination. Affects the directionality computation for the priesthood seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_interpretation_discretion, empirical, 'Degree of freedom in priestly interpretation of divine will.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of heterodox practitioners structural (they face external barriers: exile, death, property loss) or internalized (they have internalized the belief that heterodoxy is impious and self-suppress)?',
    'Post-suppression behavioral evidence: if heterodox practitioners who escape external barriers continue to avoid heterodox practice, suppression is partially internalized. If they actively practice heterodoxy upon escape, suppression is primarily structural. Historical records of dissidents who fled the priesthood''s reach and whether they maintained their alternative readings.',
    'If structural: removing the priesthood would allow immediate flourishing of heterodox practice. If internalized: removing the priesthood would not automatically restore heterodox alternatives—the constraint has been internalized as theology. This affects the long-term classification and whether the constraint can be dismantled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of heterodoxy is structural or internalized.').

omega_variable(
    reading_vs_sibling_readings,
    'This is one reading of a contested kernel (divine legitimacy substrate). Are the sibling readings (atenist_monotheistic_reading, folk_syncretistic_reading) genuinely live alternatives held by real parties in the period covered, or are they retrospective scholarly constructs?',
    'Primary source evidence of monotheistic movements (Atenism, historical context of Akhenaten) and folk practices documented in household shrine evidence, love letters, literary texts. If both are attested, the kernel is genuinely contested and all three readings are live. If one or both are absent from contemporary evidence, they are scholarly framings, not historical readings.',
    'If genuinely contested: this reading''s authority is conditional, not universal—the priesthood must defend it against live alternatives. If scholarly constructs: this reading describes the dominant system that faced no real challenge; the constraint may be better described as a mountain or rope without the need for suppression. Affects how the engine treats the reading_relations and the characterization of competing power bases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_readings, empirical, 'Whether sibling readings were live historical readings or retrospective scholarly framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(divi_tr_t8, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(divi_tr_t16, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(divi_tr_t24, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(divi_tr_t32, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(divi_be_t8, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(divi_be_t16, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(divi_be_t24, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(divi_be_t32, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(divi_su_t8, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(divi_su_t16, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(divi_su_t24, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(divi_su_t32, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(divi_su_t40, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_administrative_authority).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_property_rights).

% DUAL FORMULATION NOTE:
% This story is part of the divine_legitimacy_substrate kernel family. The amun_polytheistic_reading frames the constraint as distributed priestly interpretation of a unified cosmology. The atenist_monotheistic_reading frames it as pharaonic revelation of a single god (no priestly hierarchy). The folk_syncretistic_reading frames it as decentralized household practice. Each reading has a different ε, different beneficiary/victim structure, and different type. They are linked not as competing descriptions of one constraint but as competing readings of one contested kernel—the underlying commitment is 'what makes a pharaoh's rule legitimate,' and the three readings answer this differently. The amun_polytheistic_reading is the institutional dominant; the other two are challengers/alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
