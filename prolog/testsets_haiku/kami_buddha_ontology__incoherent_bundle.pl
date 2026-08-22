% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo: Kami-Buddha Incoherent Bundle Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha syncretism) is not a coherent ontological
 *   kernel but an institutionally sustained bundle of contradictory
 *   commitments. The system allows simultaneous fusion (honji-suijaku
 *   theories, shrine-temple pairs) and separation (domain division: kami for
 *   life, buddhas for death), hierarchy (buddha as primary, kami as
 *   derivative) and reciprocity (mutual absorption, equivalence in different
 *   contexts), systematization (coherent philosophical frameworks) and
 *   practical unsystematization (domain-switching without resolution). This
 *   reading claims the constraint IS the incoherence itself — the
 *   institutional persistence of contradiction — and that coherence-seeking
 *   readings (honji-suijaku monism, domain partition) are theoretical
 *   attempts to resolve what the system structurally prevents from being
 *   resolved. The constraint's extraction comes from institutional
 *   priesthoods maintaining authority over unresolvable questions; its
 *   suppression comes from reformers' repeated failures to impose coherence;
 *   its theater comes from the elaborate interpretive scaffolding built to
 *   paper over the contradiction while leaving it functionally intact.
 *
 * KEY AGENTS:
 *   - institutional_priesthoods (Shinto and Buddhist): maintain the dual system and benefit from unresolved status
 *   - ritual_specialists: sustain the system's practical efficacy despite theoretical incoherence
 *   - syncretist_interpretive_tradition: elaborate frameworks (honji-suijaku, etc.) that promise coherence without delivering it
 *   - doctrinal_coherence_seekers: bear cognitive burden of operating within incoherent system
 *   - separatist_reformers: attempt to impose coherence via state authority; repeatedly fail
 *   - ordinary_practitioners: navigate pragmatically; live with unresolved theoretical questions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo: Kami-Buddha Incoherent Bundle Reading").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '39b59521-8a52-4995-acfa-08f856c88acc').
narrative_ontology:cs_kernel_codification('39b59521-8a52-4995-acfa-08f856c88acc', distributed).
narrative_ontology:cs_authority_grounding('39b59521-8a52-4995-acfa-08f856c88acc', extraction).
narrative_ontology:cs_interpretation_layer_present('39b59521-8a52-4995-acfa-08f856c88acc').
narrative_ontology:cs_reading_relation('39b59521-8a52-4995-acfa-08f856c88acc', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_reading_relation('39b59521-8a52-4995-acfa-08f856c88acc', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_axiom('39b59521-8a52-4995-acfa-08f856c88acc', foundational, no_unified_ontology_necessary).
narrative_ontology:cs_axiom_status(no_unified_ontology_necessary, holdable).
narrative_ontology:cs_axiom_grounding('39b59521-8a52-4995-acfa-08f856c88acc', no_unified_ontology_necessary, instrumental).
narrative_ontology:cs_axiom('39b59521-8a52-4995-acfa-08f856c88acc', foundational, institutional_incoherence_superior_to_settled_truth).
narrative_ontology:cs_axiom_status(institutional_incoherence_superior_to_settled_truth, holdable).
narrative_ontology:cs_axiom_grounding('39b59521-8a52-4995-acfa-08f856c88acc', institutional_incoherence_superior_to_settled_truth, instrumental).
narrative_ontology:cs_reference_frame('39b59521-8a52-4995-acfa-08f856c88acc', syncretist_institutional_accommodation).
narrative_ontology:cs_drift_state('39b59521-8a52-4995-acfa-08f856c88acc', meiji_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39b59521-8a52-4995-acfa-08f856c88acc', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, institutional_priesthoods).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, ritual_specialists).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, syncretist_interpretive_tradition).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, doctrinal_coherence_seekers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, separatist_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, institutional_pragmatism_beats_theoretical_coherence).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_sustains_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shinto and Buddhist priest classes maintain parallel institutional structures and claim authority over overlapping domains (life-cycle rites, ritual efficacy, spiritual authority). They administer the system's contradictions through compartmentalization and context-switching: kami shrines handle birth, purity, and life; Buddhist temples handle death, merit, and the afterlife. The institutional stability of this arrangement — and the ritual fees, social status, and explanatory authority it grants both priesthoods — depends on NOT resolving the theoretical contradiction. Any coherent ontology would subordinate one priesthood to the other.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, institutional_priesthoods, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, institutional_priesthoods, beneficiary).

% Ritual specialists (miko, priests, monks, practitioners) occupy a stable profession managing the system's practical efficacy. They author and transmit ritual procedures that work-in-practice: a person born receives kami blessing at a shrine; when dead, receives Buddhist merit-transfer at a temple. The contradiction between kami and buddha as ontological categories is not their problem — their domain is functional efficacy, and the system delivers it. Any doctrinal resolution would destabilize the ritual grammar they have mastered.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ritual_specialists, beneficiary,
    organized, biographical, constrained, national).

% Scholars and theologians who developed honji-suijaku doctrine and other syncretist frameworks benefit from the intellectual puzzle the incoherence presents: it provides career-long interpretive work. The honji-suijaku reading — kami as suijaku (manifestations) of buddha-bodhisattva honji (original ground) — offered one resolution path, but it never achieved universal acceptance and remains one competing interpretation among many. The interpretive tradition sustains itself by elaborating frameworks that paper over the contradiction without definitively settling it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, syncretist_interpretive_tradition, beneficiary,
    organized, generational, constrained, national).

% Philosophers, theologians, and practitioners who seek a coherent ontology — a single account of what kami and buddhas ARE — find themselves unable to ground such an account in institutional practice. They invest intellectual labor attempting to systematize the contradiction (honji-suijaku, domain partition, hierarchical frameworks) but none achieves universal institutional adoption. They bear the cognitive cost of operating within an incoherent system while the institutional beneficiaries extract stability from their inability to impose coherence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, doctrinal_coherence_seekers, payer,
    moderate, biographical, constrained, national).

% From the Meiji Restoration onward, nationalist and modernist reformers attempted to separate Shinto and Buddhism definitively (shinbutsu-bunri, separation of kami and buddhas). They saw the incoherent bundle as a legacy obstacle and worked to establish Shinto as a distinct, purified national religion. Their reforms faced resistance from priesthoods, ordinary practitioners whose life-cycle rites were bound into the syncretist system, and regional institutions whose economic base depended on the overlap. The bunri reforms partially succeeded (legal/administrative separation, purification rhetoric) but failed to dislodge the practical incoherence: households and communities continued to use both systems, contradictions persisted, and the attempt to enforce coherence through state authority ultimately collapsed or fragmented.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, separatist_reformers, payer,
    powerful, generational, mobile, national).

% Households and individuals navigate the system pragmatically: they perform kami rites at shrines when seeking blessing for life, health, success; they engage Buddhist temples for funerals, memorial rites, and merit-transfer for ancestors. They do not typically invest in resolving the ontological contradiction — the system works for its purposes. But they also face costs: priests and scholars argue over what they should believe, state reformers impose competing jurisdictions, and the lack of a settled framework means no single authority can definitively answer questions about spiritual taxonomy, possession, contamination, or afterlife mechanics.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners, payer).

% Outside observers — comparative religionists, philosophers of religion, historians — examine the system as a case study in how institutional contradictions persist, how ritual efficacy can sustain theoretical incoherence, and how coherence-seeking reforms fail when they attempt to override practical functionality.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, philosophical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, institutional_priesthoods).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shinbutsu-shugo solves a practical problem: how to manage Japanese religious life when different ritual domains (birth, purity, fertility, health, life transitions vs. death, merit, afterlife, ancestor propitiation) have been culturally associated with distinct religious systems (Shinto and Buddhism). Rather than forcing a single ontology, the system coordinates by domain-switching: kami-focus for living concerns, buddha-focus for death and the afterlife. Practical efficacy — people's lives work within this framework — sustains the coordination despite theoretical contradiction.
% TRANSFER_FUNCTION: Moves ritual authority, social status, explanatory power, and economic resources (temple endowments, shrine donations, ritual fees) to institutional priesthoods (both Shinto and Buddhist) and organized ritual specialists. Ordinary practitioners transfer deference to multiple, sometimes contradictory authorities (kami priests, Buddhist monks, interpretive scholars) and accept cognitive burden (living with unresolved questions about ontological status, contamination, spiritual efficacy).
% ABSENT_VOICES: Coherence-seeking reformers (philosophers, modernists, nationalists) speak loudly but remain partially excluded: their coherent frameworks never gain universal institutional adoption, and attempts to impose coherence through state authority (Meiji shinbutsu-bunri) ultimately fail or fragment because the incoherent system's practical functionality is stronger than coherence mandates. Practitioners who want a settled, unified ontology are similarly structurally excluded — the system structurally prevents settlement. Regional institutions that depended economically on the syncretist overlap were pushed toward separation but many resisted or maintained parallel structures.
% DISAPPEARANCE_RATIONALE: If the incoherent bundle were dissolved and replaced with a single coherent ontology (say, all kami subordinated to buddha via honji-suijaku, or all buddhas secularized and kami established as sole national religious category), the institutional priesthoods would face a zero-sum allocation: one would dominate, the other would shrink. Ritual specialists' mastered procedures would be redrawn. Families' life-cycle practices would require renegotiation. The entire system of distributed authority would crystallize into hierarchy. This actually occurred partially during Meiji separatism, and the Japanese religious landscape restructured (though never completely — the original incoherence kept reasserting itself). So the answer is: arrangements depend on the contradiction and would reorganize if it were settled.
% FOUNDING_PROBLEM: The syncretism arose from the historical encounter of two religious systems (indigenous kami veneration and imported Buddhism) occupying the same geographical and cultural space for over 1,000 years. Rather than one displacing the other, they developed practical accommodation: Buddhism adopted kami as bodhisattva manifestations (honji-suijaku framework), shrine priests incorporated Buddhist rituals, temples enshrined kami, and the population used both systems. The founding problem was: how do two incompatible ontologies coexist without warfare or forced unification? The answer was: they don't, coherence-wise. But they coexist functionally because nobody forced the issue.
% FOUNDING_PROBLEM_CORROBORATION: Historians and comparative religionists (outside the benefiting priesthoods) confirm the founding problem is live: contemporary Japan still hosts unresolved tensions between kami and buddha frameworks, modern practitioners still navigate both systems, and theoretical coherence remains unsettled. Meiji-era reformers and modern separatists attest the problem is live by their repeated attempts to resolve it. The institutional priesthoods themselves attest indirectly by continuing to maintain separate institutional structures and resisting doctrinal unification.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is classified as PITON because: (1) The constraint's primary function — coordination through practical accommodation — is intact and works; (2) Extraction arises from institutional priesthoods maintaining authority over unresolved ontological questions, which ordinary people cannot definitively answer and reformers cannot definitively settle; (3) Theater is high (0.72) because an enormous amount of institutional and scholarly activity (honji-suijaku elaboration, ritual systematization, interpretive tradition) performs the function of APPEARING to resolve the contradiction while structurally preventing resolution — the more elaborate the theory, the more its ultimate incoherence is masked; (4) Suppression (0.58) is moderate-to-high because coherence-seeking is actively suppressed: Meiji separatism was imposed by state force; modern scholarly attempts at coherence never gain universal adoption; ordinary practitioners are prevented from settling the question by the institutional structures themselves; (5) Accessibility_collapse (0.41) is relatively low because the alternatives to the incoherent bundle ARE visible and have been attempted (honji-suijaku did gain adoption among some; separatism was imposed; domain partition is intellectually coherent) — the collapse is not toward incoherence but AWAY from it. Alternatives exist; they simply fail to displace the system's practical efficacy; (6) Resistance (0.71) is high because coherence-seeking movements (Meiji separatism, modernist philosophers, contemporary reformers) mount real, sustained resistance. The incoherence persists not because it goes unchallenged but because it works better than any proposed resolution. The measurement trajectory shows extractiveness rising through the 1800s–1900s (institutional priesthoods tightening control during Meiji upheaval and post-Meiji consolidation) and then plateauing — the system reached a stable extractive equilibrium where contradiction is institutionalized and profitably so.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional priesthoods' and ritual specialists' seats, this is a ROPE (or even a MOUNTAIN): the system coordinates practical religious life elegantly, works without requiring theoretical consensus, and the incoherence is not experienced as extraction — it is experienced as adaptive sophistication. From the doctrinal coherence-seekers' seat, this is a SNARE: they are trapped in an unresolvable system, their intellectual labor is continuously defeated by institutional inertia, and they cannot exit without abandoning professional identity. From the separatist reformers' seat, it is temporarily a TANGLED ROPE (coordination + enforcement of coherence) until the enforcement fails and they recognize it as a PITON that resurges when their force is relaxed. The engine's per-seat computation should reveal this divergence starkly: high institutional power and high exit flexibility (arbitrage, identity-locked for priests) should show beneficiary-ward directionality; low-moderate power and constrained exit (coherence-seekers, reformers) should show target-ward directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional priesthoods: power=institutional, exit=identity_locked (priesthood is life identity), time_horizon=generational (lineage inheritance of priest status) → low directionality (beneficiary end). They structure the system and profit from its contradictions. Ritual specialists: power=organized, exit=constrained (profession is hard to exit; ritual knowledge is specialized), time_horizon=biographical → moderate-low directionality (slight beneficiary). They benefit from the system's stability even if they don't set its rules. Doctrinal coherence-seekers: power=moderate, exit=constrained (intellectual tradition, career commitment), time_horizon=biographical → moderate-high directionality (closer to target end). They invest labor the system defeats. Separatist reformers: power=powerful (especially during Meiji when state-backed), exit=mobile (they can choose different reform agendas), time_horizon=generational (institutional change) → but mobile exit and powerful formal authority should drive LOW directionality overall. However, their failure to displace the system despite power and resources indicates the system extracts FROM their efforts — they bear the cost of attempting reform without gaining the authority to settle it. A directionality override may be warranted here: despite formal power, their inability to impose coherence despite repeated attempts suggests they are targets of the system's extractive incoherence (they pour state resources into separatism; the system resurges anyway; they achieve appearance of separation without substance).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is present and acute: the founding problem (how do two ontologies coexist?) was genuinely live and required a coordination solution. The solution was: functional accommodation without theoretical settlement. For roughly 1000 years this worked superbly — the system delivered practical efficacy without forcing impossible consensus. BUT by the modern period (1800s onward) the founding problem's STATUS shifted (empirically and institutionally): Meiji reformers and modernists established that coherence WAS achievable (honji-suijaku, domain partition, or secular Shinto nationalism) — the original problem of NEEDING COORDINATION DESPITE INCOMPATIBLE ONTOLOGIES became a problem of CHOOSING NOT TO IMPOSE COHERENCE DESPITE HAVING POWER TO DO SO. At that inflection point, the constraint mutated from ROPE (genuine coordination solving a real problem) to PITON (institutional arrangement whose primary function atrophied or reversed, maintained by inertia and ritual theater). The extraction component rose because institutional priesthoods became the primary beneficiaries of the UNSETTLED state itself rather than beneficiaries of functional coordination. The system now extracts value from the fact that nobody can definitively tell practitioners what kami and buddhas are — that remaining ambiguity is what grants interpretive authority to priesthoods, temples, and scholarly traditions. The mandate was: coordinate incompatible systems. The mandate is now obsolete (coherence is achievable; alternatives are known). The constraint persists anyway because the incoherence itself has become institutionally profitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_discursive_contradiction,
    'Is the measured incoherence a feature of the constraint''s STRUCTURE (practitioners genuinely live with unresolved ontological contradiction) or a feature of DISCOURSE (theoretical frameworks are contradictory but practitioners don''t internalize the contradiction)?',
    'Ethnographic research: interview practitioners about their actual beliefs re: what kami and buddhas are; test whether they experience contradictions or compartmentalize smoothly. Analyze actual ritual practice for evidence of genuine confusion vs. context-switching.',
    'If structural, the constraint''s extractiveness is about institutional priesthoods maintaining authority over genuine confusion (high extraction, high suppression). If discursive, the system is more of a ROPE than a PITON — ordinary practitioners cope fine, and the incoherence is a scholar''s problem, not a structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_discursive_contradiction, empirical, 'Whether incoherence is experientially real or discursively constructed.').

omega_variable(
    reformation_inevitability,
    'Will separatist pressure (shinbutsu-bunri, modernization, state mandates for coherence) eventually succeed in imposing a coherent reading, or does the system have structural immunity to coherence?',
    'Long-term institutional observation: if state power, economic incentives, and philosophical arguments have failed to displace the incoherent bundle after 150+ years of Meiji-onward pressure, the system likely has deep structural resilience. If coherence is eventually imposed, it will show that the constraint was contingent institutional inertia, not a necessary feature of how religion works.',
    'If coherence is inevitable, the constraint should be reclassified as a temporary SCAFFOLD with institutional momentum rather than a PITON — it''s on its way out but slow. If the incoherence has genuine structural resilience, it remains PITON and the extraction is more durable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_inevitability, empirical, 'Whether the incoherent bundle is a temporary institutional legacy or structurally stable.').

omega_variable(
    honji_suijaku_as_resolution_vs_rebranding,
    'Did honji-suijaku doctrine RESOLVE the contradiction between kami and buddha (making them coherent by subordinating kami to buddha), or does it merely REBRAND the same institutional incoherence under a philosophical label?',
    'Analysis of whether honji-suijaku adoption changed institutional practice, priesthood relationships, or ordinary practitioners'' understanding. Did it settle disputes or create new interpretive contests? Did it reduce theater or increase it?',
    'If honji-suijaku resolved the issue, it demonstrates that coherence CAN displace incoherence, and this reading''s classification as PITON is wrong — the system should be classified as a temporary SNARE with a honji-suijaku escape path. If honji-suijaku merely elaborated theater, it confirms that the incoherence is institutionally protected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_as_resolution_vs_rebranding, conceptual, 'Whether honji-suijaku represents genuine resolution or elaborated theater.').

omega_variable(
    kernel_reading_identity_question,
    'Is this reading — that the constraint is the incoherence itself — a coherent reading of the kami-buddha kernel, or does it collapse the kernel itself (refusing to treat it as a genuine question with possible answers)?',
    'Philosophical analysis: a reading that says ''there is no true answer, only institutional contradictions'' is itself a metaphysical claim (nominalism about ontological truth, institutionalism about stability). Test whether this reading can be compared with honji-suijaku and domain-partition on the same logical axis (all three answering ''what is the relationship?'') or whether it withdraws from the question entirely (claiming there is no relationship to get right).',
    'If this reading is on the same axis as the others (answering the kernel question), it should be classified as COEXISTS_WITH them. If it withdraws from the question entirely, the kernel itself may be malformed (the three readings don''t share a common reference point) and downstream analysis should note that the kernel is meta-theoretically contested, not just observationally contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_question, conceptual, 'Whether the incoherent-bundle reading answers the kernel question or rejects the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.64).
narrative_ontology:measurement_basis(kami_tr_t0, observed).
narrative_ontology:measurement(kami_tr_t8, kami_buddha_ontology__incoherent_bundle, theater_ratio, 8, 0.67).
narrative_ontology:measurement_basis(kami_tr_t8, observed).
narrative_ontology:measurement(kami_tr_t16, kami_buddha_ontology__incoherent_bundle, theater_ratio, 16, 0.7).
narrative_ontology:measurement_basis(kami_tr_t16, observed).
narrative_ontology:measurement(kami_tr_t24, kami_buddha_ontology__incoherent_bundle, theater_ratio, 24, 0.72).
narrative_ontology:measurement_basis(kami_tr_t24, observed).
narrative_ontology:measurement(kami_tr_t32, kami_buddha_ontology__incoherent_bundle, theater_ratio, 32, 0.72).
narrative_ontology:measurement_basis(kami_tr_t32, observed).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__incoherent_bundle, theater_ratio, 40, 0.72).
narrative_ontology:measurement_basis(kami_tr_t40, observed).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__incoherent_bundle, theater_ratio, 50, 0.72).
narrative_ontology:measurement_basis(kami_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(kami_be_t0, observed).
narrative_ontology:measurement(kami_be_t8, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(kami_be_t8, observed).
narrative_ontology:measurement(kami_be_t16, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(kami_be_t16, observed).
narrative_ontology:measurement(kami_be_t24, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(kami_be_t24, observed).
narrative_ontology:measurement(kami_be_t32, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(kami_be_t32, observed).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(kami_be_t40, observed).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(kami_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(kami_su_t0, observed).
narrative_ontology:measurement(kami_su_t8, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(kami_su_t8, observed).
narrative_ontology:measurement(kami_su_t16, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 16, 0.57).
narrative_ontology:measurement_basis(kami_su_t16, observed).
narrative_ontology:measurement(kami_su_t24, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(kami_su_t24, observed).
narrative_ontology:measurement(kami_su_t32, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(kami_su_t32, observed).
narrative_ontology:measurement(kami_su_t40, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(kami_su_t40, observed).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(kami_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).

% DUAL FORMULATION NOTE:
% This story is one reading of the kami_buddha_ontology kernel. The kernel represents a single persisting commitment (the relationship between kami and buddhas in Japanese religious practice) that different parties read differently. Three constraint stories decompose this kernel: domain_partition (kami and buddhas are distinct entities in separate domains), honji_suijaku_monism (kami are phenomenal manifestations of buddha ground), and incoherent_bundle (no single coherent reading; institutional incoherence is the constraint). Each story authorizes its reading with its own ε, beneficiary/victim structure, and type. The three stories are linked via network.affects_constraints to show they are readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
