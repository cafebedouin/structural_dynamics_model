% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse: Orthodox Literal Reading (Caste-Duty, Righteous War)
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse, read literally by orthodox
 *   Brahminical interpretation, presents a constraint that legitimates
 *   caste-based social hierarchy and justifies warrior violence through duty
 *   (dharma). Arjuna's doubt about fighting his relatives is resolved by
 *   Krishna's teaching: his caste-assigned role (kshatriya dharma) mandates
 *   participation in righteous war (dharma yuddha); violence performed in
 *   duty-bound service is not morally corrupting. Under this reading, the
 *   text consecrates the fourfold caste system (varna) as divinely ordained
 *   and makes lower-caste subordination appear cosmically necessary. This
 *   constraint is ONE reading of the contested Gita kernel. Sibling readings
 *   (Gandhian allegorical, universalist devotional) offer structurally
 *   different interpretations where violence is metaphorical or irrelevant,
 *   and caste hierarchy is rejected or transcended. This JSON instantiates
 *   ONLY the orthodox literal reading as a self-contained constraint with its
 *   own ε, beneficiaries, victims, and stakeholder structure — not as a claim
 *   about what the text 'really' means, but as a description of what this
 *   reading institutionally does.
 *
 * KEY AGENTS:
 *   - Brahminical interpretive establishment: retains monopoly on authorized Gita reading; derives institutional power and spiritual authority from exegetical control
 *   - Kshatriya warrior class: receives divine sanction for caste-duty violence; benefits from legitimation of warrior role
 *   - Lower castes (Shudras, untouchables): trapped in assigned subordinate roles; bear costs of hierarchy legitimated by this reading
 *   - War casualties and combatants outside righteous camp: treated as non-dharmic; their deaths legitimated by reading's framework
 *   - Gandhian and postcolonial challengers: excluded from orthodox authority; their competing readings are suppressed within the establishment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.81).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse: Orthodox Literal Reading (Caste-Duty, Righteous War)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '15af8004-4d7c-42d9-adce-f83ba19ce28e').
narrative_ontology:cs_kernel_codification('15af8004-4d7c-42d9-adce-f83ba19ce28e', fixed_text).
narrative_ontology:cs_authority_grounding('15af8004-4d7c-42d9-adce-f83ba19ce28e', extraction).
narrative_ontology:cs_interpretation_layer_present('15af8004-4d7c-42d9-adce-f83ba19ce28e').
narrative_ontology:cs_reading_relation('15af8004-4d7c-42d9-adce-f83ba19ce28e', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('15af8004-4d7c-42d9-adce-f83ba19ce28e', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('15af8004-4d7c-42d9-adce-f83ba19ce28e', foundational, caste_varna_divinely_ordained).
narrative_ontology:cs_axiom_status(caste_varna_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('15af8004-4d7c-42d9-adce-f83ba19ce28e', caste_varna_divinely_ordained, theological).
narrative_ontology:cs_axiom('15af8004-4d7c-42d9-adce-f83ba19ce28e', foundational, kshatriya_violence_duty_bound_legitimate).
narrative_ontology:cs_axiom_status(kshatriya_violence_duty_bound_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('15af8004-4d7c-42d9-adce-f83ba19ce28e', kshatriya_violence_duty_bound_legitimate, deontological).
narrative_ontology:cs_reference_frame('15af8004-4d7c-42d9-adce-f83ba19ce28e', vedic_cosmic_order_varna_dharma).
narrative_ontology:cs_drift_state('15af8004-4d7c-42d9-adce-f83ba19ce28e', postcolonial_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15af8004-4d7c-42d9-adce-f83ba19ce28e', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_interpretive_authority).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_sustenance).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_populations).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_combatants_opposing_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, divine_sanction_of_caste_varna_system).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, righteous_war_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Brahmin scholarly establishment and priesthood that controls authorized interpretation of the Gita text. They adjudicate what the text 'truly' means — the caste-duty reading vs. allegorical readings. They derive authority and institutional power from the monopoly on textual exegesis and the religious legitimacy that interpretation confers. Their interpretive framework perpetuates Brahmin epistemic authority over all dharmic questions.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_interpretive_authority, agenda_setter,
    institutional, civilizational, trapped, continental).

% The warrior caste whose duty (kshatriya_dharma) to fight in righteous war is directly legitimated by the text under this reading. They receive divine sanction for violence when fulfilling their birth-ordained role. Their warrior identity is constituted through this duty; exit would require denying the caste assignment itself. The text makes their violence morally and religiously permissible where alternative readings would criminalize or delegitimize it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, civilizational, identity_locked, continental).

% The institutional structure of hereditary caste-based social ordering. The constraint (the literal Gita reading) vindicates and perpetuates this structure by depicting caste duty as divinely ordained and inviolable. Lower castes remain structurally trapped in their assigned roles; the Gita reading under this orthodoxy makes that assignment appear metaphysically binding rather than socially constructed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_sustenance, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_sustenance).

% Shudras, untouchables, and other lower castes bear the costs of the caste hierarchy this reading sustains: exclusion from ritual participation, restricted occupational roles, denied access to Vedic study, ritual pollution imposed. The reading treats their subordination as divinely ordained dharma; resistance is framed as violation of cosmic law. Their trapped status is reinforced by the reading's legitimation of the hierarchy itself.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_populations, payer,
    powerless, generational, trapped, continental).

% Those killed or displaced in wars fought by kshatriyas following this reading's mandate for righteous war (dharma yuddha). They are constituted as non-dharmic, enemies of cosmic order, or bound by fate to opposition. The reading legitimates their deaths as justified by the victor's caste duty. They bear the costs (death, displacement, suffering) without access to the interpretive frame that would legitimize their own resistance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_combatants_opposing_dharma, payer,
    moderate, immediate, trapped, local).

% Interpreters and activists who read the Gita as allegory and non-violent resistance, or who reject caste hierarchy explicitly. They are structurally excluded from the orthodox interpretive authority's adjudication — their readings are treated as modernist corruption or textual misunderstanding by the establishment. They would argue the text permits liberatory readings; that argument is suppressed within the orthodox institutional framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_reformist_challengers, excluded,
    organized, generational, constrained, continental).

% Bhakti and devotional interpreters who emphasize Krishna's teaching of path-independent devotion accessible to all castes, transcending caste duty. Their reading exists as a sibling constraint but is suppressed or reframed within the orthodox establishment's dominance. They would challenge both the literalism and the caste-binding of dharma; their voices are excluded from authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_interpreters, excluded,
    organized, generational, constrained, continental).

% External authority (colonial administration, postcolonial nation-state) that observes, sometimes intervenes in, and is confronted by the constraint's operation. They record the caste system's enforcement, document violence rationalized by dharmic readings, and occasionally enact constitutional structures (like India's anti-caste-discrimination articles) that contradict the reading's premises. They are outside the religious framework but their policies reshape its enforcement terrain.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, colonial_and_postcolonial_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_interpretive_authority).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a metaphysical framework for coordinating social roles and war-fighting: tells the kshatriya warrior why their violent role is not morally corrupting, tells all participants why their assigned station is cosmically necessary, tells the Brahmin why their interpretive monopoly is legitimate. Coordinates acceptance of hierarchy by making it appear non-negotiable.
% TRANSFER_FUNCTION: Moves spiritual authority and political legitimacy upward to the Brahminical establishment (through interpretive monopoly); moves compliance and acceptance of caste-assigned duties through the lower castes; moves justification for warrior violence downward to kshatriyas; moves human life and dignity from war casualties (treated as non-dharmic or fated) to the victor's righteous cause.
% ABSENT_VOICES: Lower castes are excluded from authoritative Vedic interpretation; their own dharma-readings are not heard. Women interpreters are largely absent from the orthodox establishment. Non-Brahmin philosophical schools (Lokayata, some Buddhist critiques) are suppressed. Colonial/postcolonial state framings that contradict caste-legitimacy are treated as external impositions, not as legitimate challenges from within the tradition.
% DISAPPEARANCE_RATIONALE: If this reading and its institutional enforcement disappeared — if Brahminical monopoly on Gita interpretation was broken and lower castes could author competing readings as authoritative — the legitimation structure for hereditary caste hierarchy would lose its primary textual anchor. Social roles would need to be renegotiated on non-metaphysical grounds. Warrior violence would lose the divine sanction specific to this reading (other readings might legitimize different conflicts, but not on caste-duty grounds). The entire apparatus of caste-as-cosmic-order would require reconstruction without this text's explicit endorsement.
% FOUNDING_PROBLEM: The Kurukshetra war is historically problematic: why should Arjuna fight? Why is violence justified? The text addresses this by embedding Arjuna's conflict in a cosmic duty framework where his caste role (kshatriya) mandates warrior participation in righteous war, and where the hierarchy of castes is presented as divinely ordained and necessary for cosmic order (rita).
% FOUNDING_PROBLEM_CORROBORATION: The orthodox Brahminical establishment attests that the founding problem is live and permanently so: the justification of violence through duty is eternally applicable, not historically contingent. Gandhian and universalist interpreters attest that the founding problem has been solved or dissolved by alternative readings (the battle is metaphorical, devotion transcends duty). Postcolonial scholars and lower-caste movements attest that the founding problem itself reflects an elite anxiety (how to justify the kshatriya role and hierarchy) and that the text has been read to serve Brahminical power interests rather than to solve a universal human problem. No consensus exists outside the orthodox establishment.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the reading systematically legitimates extraction of compliance and subordination from lower castes: they are told their placement is cosmic necessity, divinely ordained, eternally valid. The constraint makes acceptance of hierarchy appear non-negotiable and resistance appear to violate cosmic law. Suppression is correspondingly high (0.78) because alternative readings are actively suppressed by Brahminical institutional authority; lower castes historically lacked literacy and access to textual interpretation, making their competing readings structurally absent from the legitimate discourse. Theater ratio (0.41) is moderate: the reading does provide a genuine (if highly contestable) metaphysical framework for understanding duty and role, but increasingly that framework is maintained performatively against postcolonial, Gandhian, and lower-caste challenges. The measurement series spans 2000 years (the interval 0–2000, each unit ~1 year) to capture pre-colonial stability (t0), early modern Brahminical retrenchment (t500–1000), colonial encounter and challenge (t1000–1500), and postcolonial transformation (t1500–2000). Extractiveness and suppression show slight rise over time as the reading becomes more actively defended against challengers rather than simply assumed. Theater rises as the constraint is increasingly maintained by rhetoric and institutional policing rather than by unchallenged consensus.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical agenda-setter and the kshatriya beneficiary experience this constraint as coordination and legitimacy; from their seats, the text solves the genuine problem of how to make sense of hierarchy and warrior duty. Lower castes and war casualties experience it as pure extraction: they are told to accept subordination and death-risk as cosmic necessity, without their own interpretive voices heard. The engine computes this divergence per-seat: the Brahmin and kshatriya seats will compute as benefiting from coordination; the lower-caste and casualty seats will compute as trapped targets. This perspectival gap is the core asymmetry the constraint embodies — one reading of the text legitimates it as coordination, another (the Gandhian reading) as extraction, and the orthodox institutional dominance keeps the second reading suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical authority (institutional power, civilizational time horizon, trapped exit) derives d near the beneficiary end: they control the constraint and collect interpretive authority. Kshatriyas (powerful, but identity-locked as warriors and bound to kshatriya_dharma) sit near symmetric or slightly beneficiary: they gain legitimation of their role but are also bound by it (cannot exit warrior duty without violating the text). Lower castes (powerless, trapped in caste assignment, generational horizon) compute as full targets (d near 1.0): they bear the costs of hierarchy and lack the power or exit to evade it. War casualties (moderate power in the immediate moment of battle, but trapped because the reading frames them as non-dharmic outsiders) also compute as targets. Excluded challengers (organized power, but constrained because they are suppressed by institutional authority) compute as partially targeted by the suppression mechanism, though they are not officially named victims — they are excluded rather than victimized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justifying Arjuna's participation in the Kurukshetra war) is contested in its status. The orthodox reading claims the problem is live: duty and dharma are eternally applicable, and every kshatriya faces analogous conflicts. Challengers claim the problem is dead or was never universally binding: the specific historical conflict is not metaphysically necessary, and alternative readings dissolve the apparent dilemma. This reading is classified as tangled_rope because it carries BOTH a genuine coordination function (explaining role and duty, making sense of hierarchy) AND asymmetric extraction (legitimating subordination and violence). The classification is not split because both elements inhere in the same reading: you cannot separate the legitimacy of the hierarchy from the justification of warrior duty — they are unified in Krishna's teaching. However, the constraint is classified at the payer/victim seat level as approaching snare: lower castes experience pure extraction without meaningful coordination benefit (the hierarchy does not solve problems for them; it perpetuates their costs). The Brahminical and kshatriya seats experience rope or coordination; the lower-caste seats experience snare. This per-seat divergence is the measurement the constraint story exists to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_intentionality,
    'Is the Gita''s text intentionally literal (Krishna actually commands caste duty and righteous war as described) or are the literal words a vehicle for timeless spiritual principles that can be read allegorically?',
    'Textual criticism comparing Gita to adjacent Mahabharata passages; author-intent analysis (impossible for historical anonymous texts); comparative study of how different Indian philosophical schools parsed the same text at its earliest reception.',
    'If intentionally literal, the constraint''s claim of textual mandate is structurally grounded; if intentionally allegorical, the literal reading is a misreading imposed by orthodox authority. This determines whether the extraction is textually justified or institutionally fabricated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_allegorical_intentionality, conceptual, 'Whether the text''s literal surface is its intended meaning or a mask for deeper principles.').

omega_variable(
    caste_cosmology_vs_social_construction,
    'Is caste hierarchy (varna system) a metaphysically real, divinely ordained structure, or is it a historically contingent social invention that the text merely describes/endorses?',
    'Comparative analysis of pre-Vedic societies; archaeological and genetic evidence for the varna system''s actual historical origin; examination of whether the constraint''s persistence requires metaphysical claims or merely institutional power.',
    'If metaphysically real and divinely ordained (as the orthodox reading claims), the constraint legitimately describes cosmic necessity. If socially constructed, the constraint is institutional violence masquerading as cosmic truth. This bifurcates the type classification: metaphysical = mountain-adjacent, social = snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_cosmology_vs_social_construction, empirical, 'Whether the varna system is a feature of cosmic order or human social history.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.78) enforced externally (Brahminical institutional authority preventing lower-caste interpretation) or is it largely internalized (lower castes have internalized the belief that their subordination is cosmic necessity and do not seek alternative readings)?',
    'Ethnographic and historical study of lower-caste dissent: where alternative readings (Bhakti movements, Dalit theology, postcolonial contestation) emerge, does their emergence immediately face violent institutional resistance (external suppression) or do they grow slowly because internalized acceptance was the barrier? Comparison of regions with strong Brahminical institutional control vs. regions where institutions were weaker.',
    'If primarily external suppression, the constraint''s persistence depends on institutional enforcement and would shift under institutional breakdown. If primarily internalized, the constraint persists through belief even without institutional enforcement; its effective suppression is higher than the structural measure suggests, and escape requires cognitive decolonization, not merely institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural (institutional authority) or internalized (belief and self-concept).').

omega_variable(
    dharmic_war_extensibility,
    'Does the reading''s doctrine of righteous war (dharma yuddha) constrain legitimate war to historically specific conflicts (the Kurukshetra war as singular and unrepeatable) or does it establish a principle that extends to any war fought by kshatriyas in defense of dharma?',
    'Textual analysis of how medieval and early modern commentators applied the doctrine; historical record of which wars were claimed as dharma yuddha and who did the claiming; examination of whether the reading permits open-ended extension of violence justification.',
    'If historically specific, the reading''s harm is bounded to the Kurukshetra context (though the hierarchy persists). If extensible, the reading becomes a template for legitimating any warrior violence in defense of caste order, making its extractiveness and suppression active across multiple historical contexts and far more consequential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharmic_war_extensibility, conceptual, 'Whether the righteous war doctrine is singular or generalizable.').

omega_variable(
    reading_incompleteness_as_omega_locus,
    'This constraint story describes the orthodox literal reading as a unified, coherent stance. But is it? Do orthodox interpreters themselves hold competing sub-readings, disagree on whether caste subordination is acceptable, or preserve escape clauses (e.g., reformed caste, bhakti access) that partially collapse the constraint''s extractiveness?',
    'Detailed analysis of actual Brahminical exegetical debates (medieval commentators like Shankara vs. Ramanuja vs. Madhva); examination of whether reformist Brahminical voices (e.g., 19th-century Hindu reform movements) modified the reading while staying within the orthodox institutional framework.',
    'If the orthodoxy is internally contested, the constraint is less unified than presented here; extractiveness might be lower if escape clauses are acknowledged. If the orthodoxy enforces a singular line and suppresses internal critique, the constraint is more thoroughly extractive and its institutional enforcement is more totalizing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompleteness_as_omega_locus, empirical, 'Whether the orthodox reading is internally coherent or contains suppressed alternatives and dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement_basis(gita_tr_t500, observed).
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1000, 0.37).
narrative_ontology:measurement_basis(gita_tr_t1000, observed).
narrative_ontology:measurement(gita_tr_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement_basis(gita_tr_t1500, observed).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(gita_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.76).
narrative_ontology:measurement_basis(gita_be_t500, observed).
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1000, 0.79).
narrative_ontology:measurement_basis(gita_be_t1000, observed).
narrative_ontology:measurement(gita_be_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1500, 0.81).
narrative_ontology:measurement_basis(gita_be_t1500, observed).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement_basis(gita_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(gita_su_t0, projected).
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.71).
narrative_ontology:measurement_basis(gita_su_t500, observed).
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1000, 0.74).
narrative_ontology:measurement_basis(gita_su_t1000, observed).
narrative_ontology:measurement(gita_su_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1500, 0.77).
narrative_ontology:measurement_basis(gita_su_t1500, observed).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement_basis(gita_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_institutional_authority_maintenance).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_cosmic_legitimation).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing the contested Gita kernel. The family includes three sibling readings of the same text: (1) Orthodox literal reading (THIS STORY): caste duty and righteous war as divinely mandated; high extractiveness (0.81), institutional beneficiary. (2) Gandhian allegorical reading: battle is metaphor for spiritual struggle; violence is internal; extractiveness lower (~0.35), liberation-oriented. (3) Universalist devotional reading: bhakti transcends caste; extractiveness medium (~0.55), devotion-centered. Each reading has different ε, different beneficiary/victim structure, different computed type per-seat. The three readings coexist as live positions held by different actors in Indian society; no single framework has unified them. They are not measured points on one constraint; they are structurally distinct constraints sharing a historical textual anchor. Links via affects_constraints trace the institutional contestation: this reading's Brahminical dominance actively suppresses the siblings, influences how the text is taught, shapes state policy through residual Brahminical cultural authority. The siblings influence this reading by providing competing legitimacy claims that the orthodox establishment must rhetorically defend against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
