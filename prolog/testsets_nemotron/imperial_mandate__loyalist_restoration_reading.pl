% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate — Loyalist Restoration Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The Meiji Restoration (1868) instantiated the loyalist restoration
 *   reading of the imperial mandate: the emperor, long confined to ritual
 *   sovereignty by the Tokugawa shogunate, was 'restored' to direct
 *   administrative rule. This reading claims the divine mandate requires the
 *   emperor to personally exercise governance — not merely reign — and that
 *   any intermediary structure (shogunate, regency, cabinet responsible to
 *   parliament) is a usurpation unless directly authorized by imperial will.
 *   The constraint operates as a tangled rope: it genuinely solves the
 *   coordination problem of fragmented sovereignty under foreign threat
 *   (beneficiaries: court faction, new bureaucrats), but does so through
 *   asymmetric extraction from the shogunal order, samurai class, and domain
 *   system (victims), enforced by a new imperial army, police, and legal
 *   codes. The theater ratio remains low-moderate because the coordination
 *   function (centralized modernization) is real and substantial, but the
 *   extraction from the old order is structural, not performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate — Loyalist Restoration Reading").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'e19eef01-222e-4a23-a6c3-19197a91126c').
narrative_ontology:cs_kernel_codification('e19eef01-222e-4a23-a6c3-19197a91126c', fixed_text).
narrative_ontology:cs_authority_grounding('e19eef01-222e-4a23-a6c3-19197a91126c', lineage).
narrative_ontology:cs_interpretation_layer_present('e19eef01-222e-4a23-a6c3-19197a91126c').
narrative_ontology:cs_reading_relation('e19eef01-222e-4a23-a6c3-19197a91126c', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('e19eef01-222e-4a23-a6c3-19197a91126c', foundational, emperor_must_personally_govern).
narrative_ontology:cs_axiom_status(emperor_must_personally_govern, holdable).
narrative_ontology:cs_axiom_grounding('e19eef01-222e-4a23-a6c3-19197a91126c', emperor_must_personally_govern, deontological).
narrative_ontology:cs_axiom('e19eef01-222e-4a23-a6c3-19197a91126c', foundational, delegation_is_usurpation).
narrative_ontology:cs_axiom_status(delegation_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('e19eef01-222e-4a23-a6c3-19197a91126c', delegation_is_usurpation, deontological).
narrative_ontology:cs_reference_frame('e19eef01-222e-4a23-a6c3-19197a91126c', ancient_direct_rule).
narrative_ontology:cs_drift_state('e19eef01-222e-4a23-a6c3-19197a91126c', tokugawa_bakufu_establishment, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e19eef01-222e-4a23-a6c3-19197a91126c', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court_faction).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, court_nobles).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_bureaucrats).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, domain_daimyo).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, domain_daimyo).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_sovereignty_unmediated).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, mandate_requires_active_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign center of the mandate, restored to direct administrative authority after centuries of shogunal rule. His legitimacy depends on personally exercising governance — rituals, appointments, edicts, foreign treaties — not merely reigning. Exit from the role is identity-locked: the mandate constitutes his being; abdication would dissolve the cosmic order he embodies.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, emperor, beneficiary).

% Court nobles and imperial loyalists who orchestrated the restoration. They staff the new bureaucracy, control access to the emperor, and direct policy. Their power derives entirely from proximity to the restored sovereign; exit means losing the only institutional platform that legitimates their authority. Some maintain domain ties as fallback, but the restoration's logic demands centralization.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court_faction, beneficiary,
    organized, biographical, constrained, national).

% Newly appointed officials — many from lower samurai or court backgrounds — who administer the centralized state. They gain careers and status from the imperial administration. Exit is constrained: they have invested in the new system's language, law, and meritocratic rhetoric; returning to domain service or traditional roles is professionally and ideologically costly.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_bureaucrats, beneficiary,
    moderate, biographical, constrained, national).

% The Tokugawa bakufu's administrative apparatus — roju, bugyo, hatamoto — whose entire institutional world is declared illegitimate. They lose offices, stipends, and the legal framework that defined their power. Some are co-opted into the new bureaucracy, but the restoration's founding claim requires their structural displacement. Resistance means civil war; submission means erasure of their institutional identity.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate_officials, payer,
    powerful, immediate, trapped, national).

% The warrior class whose status, stipends, and social role are anchored in the shogunal order. The restoration abolishes hereditary stipends (chitsuroku), replaces domain armies with a national conscript force, and redefines honor around imperial loyalty rather than lordly service. Exit options: adapt to new military/bureaucratic roles (constrained by skills and ideology), rebel (Satsuma Rebellion), or decline into poverty. The class as a collective pays the transition's cost.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    organized, biographical, constrained, national).

% Feudal lords who surrender domain registers (hanseki hokan) and later domains themselves (haihan chiken). They receive kazoku peerages and government bonds as compensation — a partial beneficiary position — but lose autonomous governance, military forces, and the personal loyalty of their samurai. Their exit is constrained: refusal triggers imperial army intervention; acceptance makes them dependent on the central court they helped restore.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, domain_daimyo, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, domain_daimyo, beneficiary).

% Peasants, merchants, artisans who bear taxation, conscription, and educational mandates of the new state without representation in the restoration's deliberations. The rhetoric of 'imperial restoration' claims to act for the people (min), but the actual constraint restructures their obligations upward. No organized voice in the constitutional settlement; resistance is localized and suppressed.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, commoner_population, excluded,
    powerless, immediate, trapped, local).

% Western treaty powers (Britain, France, US, Russia, etc.) who negotiate with the restored imperial government. They require a recognized sovereign counterparty for treaty enforcement and most-favored-nation clauses. The restoration gives them a centralized interlocutor, but they also exert pressure for legal reforms, extraterritoriality, and market access. Their analytical seat tracks whether the new regime can actually enforce its sovereignty.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation of sovereign authority under the bakufu: multiple power centers (shogun, emperor, daimyo) claiming overlapping legitimacy, producing policy paralysis in the face of foreign pressure. The restoration creates a single decision nexus — the emperor's court — capable of issuing binding orders nationwide, negotiating treaties, and mobilizing resources for modernization.
% TRANSFER_FUNCTION: Moves governing authority, fiscal resources, and status capital from the shogunal/daimyo system to the imperial center. The emperor gains administrative control; court faction and bureaucrats gain offices and policy control; daimyo lose domains but gain peerages and bonds; samurai lose stipends and status; commoners gain new tax/conscription obligations. The transfer is justified as 'restoration' but operates as centralized state-building.
% ABSENT_VOICES: Commoner populations (peasants, urban poor) who would object to conscription, land tax reform, and loss of traditional communal autonomy. Regional domain populations whose loyalty was to local lords, not a distant emperor. Women of all classes, entirely absent from the political settlement. These voices are structurally excluded — the restoration's rhetoric speaks 'for the people' while constructing a state that extracts from them without representation.
% DISAPPEARANCE_RATIONALE: If the loyalist restoration constraint vanished overnight, the centralized Meiji state would lose its legitimating core. The emperor would revert to ritual figurehead; the bureaucracy would lose its mandate; the legal equality of four classes (shinokosho) would collapse; the conscript army would lose its imperial oath. The political order would fragment into competing regional militarists, court factions, and possibly restored bakufu remnants — a rearrangement, not continuity.
% FOUNDING_PROBLEM: The Tokugawa bakufu's inability to respond coherently to Western military and diplomatic pressure (Black Ships, unequal treaties) while maintaining domestic legitimacy. The shogun claimed to act for the emperor but could not secure imperial sanction for treaties; daimyo pursued independent foreign contacts; the emperor remained ritually sovereign but administratively void. The founding problem: how to concentrate sovereign decision-making in a single authority capable of treaty negotiation, military modernization, and fiscal centralization.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist leaders (Iwakura, Okubo, Saigo) attest the problem was real and required imperial restoration — their memoirs and the Meiji Constitution's preamble. Bakufu loyalists and domain historians (e.g., Tokugawa Yoshinobu's account) attest the shogunate was already reforming (French military mission, Kobe port opening) and the restoration was a coup, not a necessity. Foreign diplomats (Parkes, Roche) report the bakufu was a functional treaty partner. The problem's status is contested because the restoration's victors wrote the history, but the losing side's institutional memory and foreign observers offer a coherent counter-narrative.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the massive transfer of resources, status, and authority from the bakufu/daimyo/samurai system to the imperial center — land tax reform (3% of land value), abolition of stipends, conscription, and centralized fiscal control. Suppression (0.72) captures the active enforcement: Boshin War (1868-69), suppression of samurai rebellions (Saga, Satsuma), Peace Preservation Laws, and the constitutional structure that reserves sovereignty to the emperor (Article 1). Theater ratio (0.25) is relatively low because the state-building achievements (railways, education, industry, military) are genuine coordination outputs, not mere spectacle — but the 'imperial will' framing increasingly masks oligarchic (genro) decision-making. Accessibility collapse (0.78) is high: once the restoration succeeds, alternative political imaginations (bakufu reform, domain confederation, republicanism) become structurally difficult to articulate within the imperial vocabulary. Resistance (0.55) is moderate: armed rebellions were crushed, but intellectual resistance (People's Rights Movement, Minponshugi) persisted and forced constitutional concessions.
 *
 * PERSPECTIVAL GAP:
 *   From the emperor/court seat, the constraint is genuine coordination restoring cosmic and political order. From the samurai/daimyo seat, it is revolutionary expropriation cloaked in restoration rhetoric. From the commoner seat, it is a new extraction layer replacing the old. The engine computes these divergences from the declared power/exit/role structure — the claimed type (tangled_rope) acknowledges both the coordination and extraction as structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor and court faction are structural beneficiaries (d ~ 0.15): they gain authority, resources, and legitimacy from the constraint's operation. Imperial bureaucrats are moderate beneficiaries (d ~ 0.35): they gain careers but are instrumental to the center's will. Shogunate officials are full targets (d ~ 0.95): their entire institutional world is erased. Samurai class are high targets (d ~ 0.8): they lose status, stipends, and monopoly on violence. Daimyo are dual-positioned (d ~ 0.55): they lose domains but gain peerages and bonds — a managed co-optation. Commoners are trapped payers (d ~ 0.85): they bear new taxes and conscription without representation. Foreign powers are analytical observers (d ~ 0.5): they engage the constraint as a sovereign counterparty but extract treaty concessions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign threat + fragmented sovereignty) was genuinely acute in 1868. By 1889 (Meiji Constitution), the coordination function — centralized state capable of treaty revision and military modernization — was substantially achieved. The mandate's continuation as 'imperial sovereignty' (Article 1) after the founding problem's resolution is the mandatrophy: the constraint persists because the imperial institution and its beneficiaries (court, bureaucracy, military, oligarchs) have no incentive to declare victory and transition to a scaffold with sunset. The 'restoration' becomes a permanent constitutional principle, not a transitional measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_naturalness_ambiguity,
    'Is the ''divine mandate requiring unmediated exercise'' a genuine structural feature of Japanese political cosmology, or a constructed claim by the restoration faction to delegitimize the bakufu?',
    'Comparative analysis of pre-Meiji political thought: did Nihon Shoki, Jinnō Shōtōki, or Mito School texts assert that the emperor MUST personally govern, or only that he is the source of legitimacy? Examination of whether early Meiji leaders (Iwakura, Okubo) themselves believed the cosmological claim or deployed it instrumentally.',
    'If cosmologically genuine, the constraint approaches mountain status for its adherents (ε invariant across readings). If instrumentally constructed, the high ε and suppression are the constraint''s actual structure — a tangled rope whose ''natural law'' framing is extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_naturalness_ambiguity, conceptual, 'Whether the mandate''s ''unmediated exercise'' requirement is cosmological necessity or political construction.').

omega_variable(
    coordination_extraction_boundary,
    'How much of the measured extraction (land tax, conscription, abolition of stipends) was structurally necessary for the coordination function (centralized modernization), and how much was distributional choice by the beneficiaries?',
    'Counterfactual fiscal-military modeling: could a centralized Japanese state have achieved treaty revision and industrialization with a less extractive transition (e.g., phased stipend conversion, domain-retention federalism)? Comparison with Meiji oligarchs'' own debates (Okuma''s federalism proposal, Itagaki''s popular rights movement).',
    'If extraction was largely necessary for coordination, the tangled_rope classification is structurally accurate. If extraction substantially exceeded coordination requirements, the constraint leans toward snare — the coordination function is cover for a distributional coup.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the extraction intensity was coordination-necessary or beneficiary-chosen.').

omega_variable(
    kernel_reading_relation,
    'Does the loyalist_restoration_reading logically foreclose the bakufu_delegation_reading, or do they coexist as competing interpretations of an ambiguous kernel?',
    'Textual analysis of the kernel''s canonical sources (Nihon Shoki, imperial edicts, bakufu laws): does any authoritative text assert that delegation of administrative authority violates the mandate''s terms, or is the mandate silent on the governance/legitimacy distinction? Historical analysis of whether Tokugawa Ieyasu sought and received explicit imperial delegation (the ''shogun appointment'' ceremony).',
    'If forecloses: the two readings are mutually exclusive within any single framework — the Meiji Constitution''s adoption of loyalist reading structurally excludes delegation logic. If coexists_with: both readings remain live in different institutional contexts (e.g., court ritual vs. administrative law), and the kernel''s ambiguity is a persistent structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation, conceptual, 'Structural relationship between the two readings of the imperial_mandate kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative political imaginations (bakufu legitimacy, domain autonomy, republicanism) primarily structural (laws, police, army) or internalized (subjects adopting imperial subjectivity, ''kokutai'' as identity)?',
    'Post-1945 trajectory: when the structural enforcement (Meiji Constitution, Peace Preservation Law) was removed by Allied occupation, did the constraint''s suppression persist in internalized form (emperor worship, national polity discourse)? Analysis of pre-1945 thought control: was ''kokutai'' enforced externally or cultivated internally through education/shrine system?',
    'If substantially internalized, the effective suppression exceeds the structural measure — the constraint''s extraction persists after its enforcement machinery is formally dismantled. This would elevate the constraint toward snare classification in retrospective analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the imperial mandate''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1868, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(impe_tr_t1873, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1873, 0.18).
narrative_ontology:measurement(impe_tr_t1877, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1877, 0.22).
narrative_ontology:measurement(impe_tr_t1889, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1889, 0.24).
narrative_ontology:measurement(impe_tr_t1895, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1895, 0.25).
narrative_ontology:measurement(impe_tr_t1905, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1905, 0.25).
narrative_ontology:measurement(impe_tr_t1912, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1912, 0.25).

% Extraction over time
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.45).
narrative_ontology:measurement(impe_be_t1873, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1873, 0.55).
narrative_ontology:measurement(impe_be_t1877, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1877, 0.62).
narrative_ontology:measurement(impe_be_t1889, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1889, 0.65).
narrative_ontology:measurement(impe_be_t1895, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1895, 0.67).
narrative_ontology:measurement(impe_be_t1905, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1905, 0.68).
narrative_ontology:measurement(impe_be_t1912, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1912, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.65).
narrative_ontology:measurement(impe_su_t1873, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1873, 0.7).
narrative_ontology:measurement(impe_su_t1877, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1877, 0.78).
narrative_ontology:measurement(impe_su_t1889, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1889, 0.72).
narrative_ontology:measurement(impe_su_t1895, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1895, 0.7).
narrative_ontology:measurement(impe_su_t1905, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1905, 0.7).
narrative_ontology:measurement(impe_su_t1912, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1912, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, meiji_constitution_1889).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, kokutai_ideology).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, zaibatsu_state_capture).

% DUAL FORMULATION NOTE:
% This constraint and bakufu_delegation_reading form the imperial_mandate kernel family. The loyalist reading claims the mandate requires unmediated imperial governance; the delegation reading claims the mandate operates through legitimate institutional delegation. They are linked by network.affects_constraints and share the kernel_id 'imperial_mandate'. The ε values differ substantially: loyalist reading ε=0.68 (high extraction from old order); delegation reading ε≈0.25 (bakufu as functional coordinator with moderate extraction). The decomposition follows the ε-invariance principle: same label ('imperial mandate'), different structural claims, different ε, different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
