% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Reading of Sovereign Legitimacy (Hereditary Divine-Right Authority)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story authors the monarchical reading of the sovereign legitimacy
 *   kernel: authority flows downward, from a sovereign whose right is
 *   inherited by bloodline and confirmed by tradition and divine sanction, to
 *   subjects who owe obligation upward. It is one of three readings of a
 *   single contested kernel about where legitimate authority originates. The
 *   republican reading (authority flows upward from popular consent) and the
 *   constitutional-hybrid reading (dual-sourced: ceremonial inheritance plus
 *   delegated political authority mediated by law) are separate constraints,
 *   not alternative measurements of this one — each has its own ε,
 *   beneficiary/victim structure, and classification, per the ε-invariance
 *   principle. This story's ε is authored strictly for the standing
 *   monarchical arrangement as this reading's own tradition presents it
 *   (divine sanction, bloodline continuity, ritual validation), not for any
 *   hypothetical consent-based alternative.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_dynasty: agenda_setter/beneficiary (institutional/arbitrage) — sets succession rule and collects tribute
 *   - aristocratic_hierarchy: beneficiary/agenda_setter (powerful/constrained) — administers locally, invested in doctrine's persistence
 *   - established_clergy: beneficiary/agenda_setter (organized/constrained) — performs ritual legitimation, receives land and protected status
 *   - commoner_subjects: payer (powerless/trapped) — bears taxation and obligation with no legitimacy voice
 *   - excluded_cadet_branches: payer (moderate/constrained) — plausible claimants excluded by succession rule, source of contest risk
 *   - merchant_and_artisan_classes: payer/excluded (moderate/constrained) — generates wealth extracted without legitimacy access
 *   - constitutional_and_republican_theorists: excluded (moderate/trapped) — competing legitimacy claim treated as sedition
 *   - constitutional_historians: observer (analytical) — assesses coordination vs. extraction across polities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.86).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Reading of Sovereign Legitimacy (Hereditary Divine-Right Authority)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'd1d1cb0b-4dde-4a9f-bd10-6695690a602d').
narrative_ontology:cs_kernel_codification('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', distributed).
narrative_ontology:cs_authority_grounding('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', lineage).
narrative_ontology:cs_interpretation_layer_present('d1d1cb0b-4dde-4a9f-bd10-6695690a602d').
narrative_ontology:cs_reading_relation('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', foundational, authority_originates_in_bloodline_and_divine_sanction).
narrative_ontology:cs_axiom_status(authority_originates_in_bloodline_and_divine_sanction, holdable).
narrative_ontology:cs_axiom_grounding('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', authority_originates_in_bloodline_and_divine_sanction, theological).
narrative_ontology:cs_axiom('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', secondary, ritual_continuity_constitutes_legitimacy_transfer).
narrative_ontology:cs_axiom_status(ritual_continuity_constitutes_legitimacy_transfer, holdable).
narrative_ontology:cs_axiom_grounding('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', ritual_continuity_constitutes_legitimacy_transfer, conventional).
narrative_ontology:cs_reference_frame('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', divine_right_hereditary_settlement).
narrative_ontology:cs_drift_state('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', post_enlightenment_constitutionalism, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d1d1cb0b-4dde-4a9f-bd10-6695690a602d', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_clergy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, commoner_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_cadet_branches).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, merchant_and_artisan_classes).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the throne by claimed bloodline right, administers succession law, controls the ritual apparatus (coronation, anointment) that validates its own authority, and collects tribute, taxation authority, and land rents flowing from subjects' obligation to the crown. Can revise succession custom to its own advantage and faces no formal mechanism by which subjects could remove it short of rebellion.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty, beneficiary).

% Holds inherited titles and land grants that derive their legitimacy from the same bloodline logic that legitimates the crown; administers local justice and taxation on the sovereign's behalf in exchange for guaranteed status. Their exit from the arrangement would mean forfeiting title and land, so they are heavily invested in maintaining the doctrine even where royal favor shifts against them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, agenda_setter).

% Performs the ritual validation (coronation rites, anointment, doctrinal pronouncement of divine sanction) that gives the bloodline claim its transcendent warrant, and receives land, tithes, and protected legal status in return. Their institutional survival is bound to the continued public credibility of divine sanction as a legitimacy source.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_clergy, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, established_clergy, agenda_setter).

% Owe taxation, labor obligation, and military service to a ruling structure whose legitimacy they had no part in establishing and cannot revoke. Alternative legitimacy claims (that authority should derive from consent or merit) are suppressed as heresy or treason. Exit means emigration, which is itself often legally restricted or economically infeasible.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, commoner_subjects, payer,
    powerless, biographical, trapped, national).

% Relatives of the ruling house with plausible bloodline claims but excluded from the succession line by primogeniture or other succession rules. They bear the cost of a system that could have elevated them but did not, and their existence is the structural seed of succession contests and civil war.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_cadet_branches, payer,
    moderate, generational, constrained, national).

% Generate the wealth taxed and appropriated by crown and aristocracy but hold no bloodline claim to authority and are formally excluded from the legitimacy structure regardless of economic contribution or competence. Some can purchase limited status accommodation but cannot enter the legitimating bloodline itself.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, merchant_and_artisan_classes, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, merchant_and_artisan_classes, excluded).

% Argue that legitimacy should flow from consent, contract, or demonstrated competence rather than bloodline and ritual. Under the monarchical reading their position is treated as sedition or heresy against divine order; they are structurally excluded from the legitimacy conversation the sovereign and clergy control, even though their objection is precisely to the arrangement described here.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_and_republican_theorists, excluded,
    moderate, generational, trapped, national).

% Study succession crises, coronation ritual, and the doctrinal history of divine right across polities to assess whether bloodline-and-ritual legitimacy functions as genuine coordination (stable succession, reduced factional violence) or primarily as extraction dressed in transcendent language.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Succession-by-bloodline solves a genuine coordination problem: it provides a determinate, low-negotiation-cost answer to 'who rules next' that avoids a fresh, potentially violent contest over authority at every transition, and the ritual apparatus provides a widely legible signal of who holds legitimate authority so that officials, subjects, and foreign powers can coordinate expectations quickly.
% TRANSFER_FUNCTION: Moves taxation, land rent, labor obligation, and military service from commoner subjects and productive classes upward to the crown, aristocracy, and clergy, in exchange for the coordination benefit of settled succession and the ideological benefit of a transcendently sanctioned social order; excluded cadet branches and non-bloodline elites are denied entry into the receiving end of that transfer regardless of capability or contribution.
% ABSENT_VOICES: Constitutional and republican theorists who hold that legitimacy should derive from consent are structurally excluded from the legitimating conversation — under this reading their claims are heresy or treason rather than a competing political theory, so they never enter the room where legitimacy is adjudicated. Excluded cadet branches similarly have no institutional channel to contest their exclusion short of rebellion.
% DISAPPEARANCE_RATIONALE: If bloodline-and-divine-sanction legitimacy vanished overnight without a replacement legitimacy structure in place, succession would become an open contest, the aristocracy's land and title claims would lose their grounding, clerical authority over political legitimation would collapse, and taxation/obligation flows currently justified by inherited right would require an entirely new justificatory basis — the entire visible order of ranks, obligations, and offices is built on this claim.
% FOUNDING_PROBLEM: Pre-state and early-state societies faced recurring, often violent contests over who held authority after a ruler's death or incapacity; bloodline succession rules, backed by ritual and divine sanction, offered a way to settle the question in advance and reduce the frequency and intensity of succession warfare.
% FOUNDING_PROBLEM_CORROBORATION: Court chroniclers, royal historians, and the clergy themselves attest the founding problem (chaotic succession violence) remains a live danger requiring hereditary settlement. Constitutional historians and comparative political scientists, writing from outside the beneficiary set, attest that many polities have since solved orderly succession and peaceful transfer of power through consent-based or codified constitutional mechanisms without bloodline exclusivity, suggesting the original coordination problem is now solvable by non-monarchical means and that bloodline-and-divine-sanction persistence functions increasingly as status preservation for the ruling and aristocratic classes rather than as the only available solution to succession violence.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is high but not maximal: the succession-certainty and coordination benefit is real (reduced frequency of violent power struggles at transition), so this is authored as tangled_rope rather than pure snare — there is a genuine coordination function riding alongside asymmetric extraction. Suppression (0.86) is authored higher than extraction because the doctrine's persistence depends heavily on actively suppressing rival legitimacy claims (treason and heresy law against consent-based theories) rather than on subjects' active preference for the arrangement. Theater ratio rises across the measured interval (0.18 to 0.42) as ritual performance (coronation pageantry, doctrinal pronouncement) increasingly substitutes for the arrangement's original coordination function once competing legitimacy theories (republican, constitutional) become intellectually available — the ritual has to work harder to hold ground it once held by default. Accessibility collapse (0.72) reflects that once the doctrine's ritual-and-bloodline logic is internalized, alternative legitimacy claims become nearly unthinkable within the tradition's own frame, though not so completely as a true mountain (resistance, 0.58, remains substantial from excluded classes and rival theorists).
 *
 * PERSPECTIVAL GAP:
 *   From the dynasty's and clergy's seats, this is coordination they administer and are entitled to by transcendent and traditional warrant — succession certainty benefiting the whole polity. From commoner_subjects' and excluded_cadet_branches' seats, the identical structure is enforced extraction backed by suppression of any rival account of where authority comes from. The engine computes these divergent seat classifications from the declared power/exit/role data; this story does not adjudicate between them by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary dynasty and aristocracy are declared beneficiaries who also set the agenda — this gives them the lowest directionality (nearest full beneficiary), amplified further by arbitrage/constrained exit options that let them exit unfavorable local arrangements while subjects cannot. Clergy occupy a similar beneficiary-agenda_setter dual role, tied to the doctrine's continued public credibility. Commoner subjects, excluded cadet branches, and merchant/artisan classes are declared victims (role: payer) with trapped or constrained exit — this pushes their directionality toward the full-target end, and the engine should compute substantially elevated effective extraction for the powerless, trapped commoner_subjects seat relative to the moderate-power, constrained merchant class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration answers are structured to expose a mandatrophy candidate: the dynasty and clergy attest the founding problem (succession violence) is still live and requires bloodline settlement, while constitutional historians attest that comparative political history has solved orderly succession through non-hereditary means. This status=contested paired with disappearance_verdict=world_rearranges is the mismatch signal the consumer reads (not the narrative itself as a claim) — it flags this arrangement as a strong candidate for having substituted status-preservation for its original coordination function, without asserting the mandatrophy is resolved outright, since the reading's own tradition still holds the founding problem live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_naturalness_vs_construction,
    'Is divine sanction of the sovereign a genuine transcendent fact this reading''s tradition takes to be real, or a constructed legitimating narrative maintained because it benefits the dynasty, aristocracy, and clergy who administer and profit from it?',
    'Comparative religious-political history: track whether divine-right doctrine content and enforcement intensity shift in ways that track ruling-class interest (e.g., doctrine hardening exactly when succession is contested or when rival legitimacy theories gain traction) versus tracking independent theological development.',
    'If doctrine content tracks ruling-class interest rather than independent theological reasoning, this substantially strengthens the tangled_rope reading (extraction wearing a naturalized justification) over any claim that the arrangement is closer to natural or inevitable order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_naturalness_vs_construction, conceptual, 'Whether divine sanction functions as genuine warrant or constructed cover for beneficiary interest.').

omega_variable(
    succession_stability_vs_contest_vulnerability,
    'Does bloodline succession actually reduce total violence and instability over the long run compared to available alternatives, or does it merely relocate violence into periodic, high-intensity succession crises (contested successions, wars of succession) whenever bloodline claims are ambiguous?',
    'Historical frequency and severity analysis of succession crises and civil wars across monarchical polities, compared against comparable-era polities using non-hereditary transition mechanisms.',
    'If bloodline succession primarily relocates rather than reduces violence, the genuine-coordination-function claim underlying this reading''s tangled_rope classification (rather than pure snare) is substantially weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_vs_contest_vulnerability, empirical, 'Whether hereditary succession genuinely reduces conflict or merely defers and concentrates it.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis the sovereign''s personal authority (the obvious framing used here) or the deeper legitimating claim that bloodline-plus-ritual tracks a real transcendent order (the less obvious framing, one level up, that the sovereign''s authority itself depends on for its warrant)?',
    'Compare classification outcomes under both framings: does authoring ε for ''the sovereign''s rule'' versus ''the doctrine that bloodline tracks divine will'' produce different beneficiary sets or different suppression profiles?',
    'If the two framings diverge in classification, the deeper doctrinal framing may deserve its own decomposed constraint story (per the ε-invariance principle) rather than being folded into this one; this story adopts the sovereign''s-personal-authority framing as primary because that is the operative legitimacy claim subjects and rival claimants actually contest in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether sovereign-authority framing and doctrine-of-bloodline framing are the same constraint or should be decomposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__monarchical_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__monarchical_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__monarchical_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__monarchical_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__monarchical_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__monarchical_reading, suppression_requirement, 80, 0.84).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.1).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the sovereign_legitimacy kernel. republican_reading authors legitimacy as flowing upward from consent (low ε for the arrangement it describes, as that reading sees it — a fundamentally different beneficiary/victim structure with no hereditary class capturing rents). constitutional_hybrid_reading authors a dual-sourced structure where ceremonial and political authority are split and mediated by law (moderate ε, narrower victim set limited to whichever authority boundary is contested). This monarchical_reading authors the highest ε and suppression of the three, reflecting its structural commitment to bloodline-and-ritual exclusivity and active suppression of the other two readings' legitimacy claims as illegitimate. All three share the same underlying kernel object (the question of where legitimate authority originates) but are authored as distinct constraints per the ε-invariance principle, since measuring 'sovereign legitimacy' under each reading's own lights yields incompatible ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
