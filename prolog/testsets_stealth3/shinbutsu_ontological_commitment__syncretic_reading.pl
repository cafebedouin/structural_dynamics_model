% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-suijaku Syncretic Settlement: Kami as Traces of the Buddha Ground
 *   domain: religious/ontological/institutional
 *
 * SUMMARY:
 *   From roughly the tenth century until 1868, the dominant Japanese
 *   religious settlement held that the kami — the indigenous deities of
 *   shrine cult — are provisional local manifestations (suijaku, traces) of
 *   buddhas and bodhisattvas, who constitute the original ground (honji).
 *   This honji-suijaku metaphysics, elaborated by Tendai (Sannō ichijitsu
 *   shintō) and Shingon (Ryōbu shintō) lineages and adopted by the later
 *   Kamakura schools, integrated shrine cult and Buddhist soteriology into
 *   one cosmological order: Amaterasu was read as a trace of Dainichi,
 *   Hachiman as a bodhisattva, and every major shrine acquired an attached
 *   temple complex (jingūji) with Buddhist clerics, rites, and
 *   administration. The arrangement genuinely solved a coordination problem —
 *   two incommensurable cultic authorities coexisting in one ritual field —
 *   and delivered real benefits to shrines, worshippers, and the court. It
 *   also entrenched a hierarchy: the Buddhist establishment sat at the
 *   original-ground end, controlling shrine ordination, appointments,
 *   revenues, and doctrine, while shrine lineages bore the subordination of
 *   their kami to Buddhist precedence. This story instantiates the syncretic
 *   reading of the shinbutsu_ontological_commitment kernel — the reading
 *   under which a stable, coherent ontological commitment existed and
 *   operated as described. The sibling readings (partition, incoherence) are
 *   separate constraints with their own ε; see the kernel omega and the
 *   network note. KEY AGENTS (by structural relationship): -
 *   buddhist_monastic_establishments: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — authored the doctrine, administers the
 *   shrine-temple complexes, receives the integration's revenues -
 *   shrine_priesthoods: principal payer (moderate/constrained) — bears the
 *   subordination of kami and shrine autonomy to Buddhist precedence -
 *   kami_worshiping_communities: near-symmetric dual position
 *   (powerless/constrained) — receives integration benefits and pays through
 *   the same structure - imperial_court: beneficiary with costs
 *   (powerful/constrained) — gains doctrinal coherence linking imperial cult
 *   to Buddhist legitimacy; pays endowments and absorbs Ise's doctrinal
 *   subordination - warrior_governments: beneficiaries (powerful/mobile) —
 *   purchase legitimation and governance infrastructure through patronage -
 *   independent_shinto_lineages: payer-resisters (organized/constrained) —
 *   Watarai and Yoshida movements mounting reverse doctrines from inside the
 *   arrangement - confucian_kokugaku_scholars: excluded voices
 *   (moderate/constrained) — objected for centuries from outside the
 *   doctrinal establishment - meiji_separation_reformers: analytical observer
 *   (institutional/analytical) — the external force that terminated the
 *   arrangement in 1868
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.62).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-suijaku Syncretic Settlement: Kami as Traces of the Buddha Ground").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/ontological/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e').
narrative_ontology:cs_kernel_codification('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', fixed_text).
narrative_ontology:cs_authority_grounding('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', extraction).
narrative_ontology:cs_interpretation_layer_present('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e').
narrative_ontology:cs_reading_relation('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', foundational, kami_are_manifest_traces_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_manifest_traces_of_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', kami_are_manifest_traces_of_buddhas, theological).
narrative_ontology:cs_axiom('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', foundational, buddha_ground_precedes_kami_trace).
narrative_ontology:cs_axiom_status(buddha_ground_precedes_kami_trace, holdable).
narrative_ontology:cs_axiom_grounding('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', buddha_ground_precedes_kami_trace, theological).
narrative_ontology:cs_axiom('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', secondary, shrine_cult_valid_under_buddhist_administration).
narrative_ontology:cs_axiom_status(shrine_cult_valid_under_buddhist_administration, holdable).
narrative_ontology:cs_axiom_grounding('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', shrine_cult_valid_under_buddhist_administration, conventional).
narrative_ontology:cs_reference_frame('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', buddha_ground_kami_trace_hierarchy).
narrative_ontology:cs_drift_state('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', late_edo_nativist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d7dc0c0-fac0-4285-ae0d-4c6c2d44231e', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, warrior_governments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, kami_worshiping_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, independent_shinto_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, kami_worshiping_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, dharmakaya_manifestation_theory).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, ryobu_shinto_synthesis).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, sanno_ichijitsu_shinto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and transmit the doctrine that kami are traces of buddhas; administer the shrine-temple complexes (jingūji) where Buddhist clerics, rites, and offices sit inside major shrines; control ordination of shrine priests, appointment to shrine offices, and a substantial share of shrine revenues and land income. New Buddhist schools adopt the framework on entry, and the corporations shift doctrine and patronage as patrons change — the original-ground end of the hierarchy is the apex their doctrine defines.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments, beneficiary).

% Hereditary lineages serving the major shrines. Their deities are doctrinally valid only as provisional traces of buddhas; their legitimacy, rank, and in many cases their appointments run through Buddhist institutions; their revenues are shared with or channeled through the temple complexes attached to their shrines. Leaving the framework means losing legitimacy, rank, and income; staying means administering a cult whose own governing doctrine ranks it second. Some lineages spent centuries building reverse doctrines from inside.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shrine_priesthoods, payer,
    moderate, generational, constrained, national).

% Villages, pilgrimage constituencies, and local cults. They receive the arrangement's integration: their kami gain soteriological depth, their festivals sit inside a national ritual economy, their shrines gain protection and prestige through temple affiliation. They also pay — offerings, tithes, and labor flow through the combined complexes — and their deities are reclassified in the process, though most never encounter the doctrine except as iconography and festival order.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, kami_worshiping_communities, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, kami_worshiping_communities, payer).

% Patron and beneficiary of the unified order: doctrinal coherence links the imperial cult to Buddhist legitimacy, and the court's rites, oracles, and accession ceremonies operate inside the arrangement. It pays through endowments and through the doctrinal ranking of its own ancestress — Amaterasu as trace of Dainichi — a subordination the Ise priesthood resisted for centuries on the court's behalf and sometimes against its preference.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, payer).

% The Kamakura, Muromachi, and Tokugawa bakufu patronized the combined complexes and used the shrine-temple network as governance infrastructure: land confirmation, dispute mediation, and legitimation ran through institutions the arrangement held together. Their patronage was purchased voluntarily and could be redirected; the warrior houses sat outside the doctrinal hierarchy while feeding it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, warrior_governments, beneficiary,
    powerful, generational, mobile, national).

% Watarai Shinto at Ise and, later, the Yoshida house mounted doctrines reversing the hierarchy — kami original, buddhas traces (hanhon-hisuijaku). They operated inside the arrangement they contested, bore its legitimacy costs, and were marginalized for generations; by the Edo period the Yoshida house administered shrine ranks, a partial capture of the enforcement machinery by the reverse doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, independent_shinto_lineages, payer,
    organized, generational, constrained, national).

% Confucian scholars from the seventeenth century and nativist kokugaku scholars from the eighteenth objected that the arrangement drained the realm toward foreign institutions and buried the kami under Buddhist metaphysics. They stood outside the temple-shrine doctrinal establishment with no seat in its councils; their critique became actionable only when the political order sustaining the arrangement collapsed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, confucian_kokugaku_scholars, excluded,
    moderate, biographical, constrained, national).

% The Meiji state's reformers issued the 1868 separation edicts (shinbutsu bunri), dissolving shrine-temple complexes, expelling Buddhist clerics from shrines, and reconstructing Shinto as an independent national cult. They did not administer the arrangement they destroyed; they stand at the interval's end as the external force that terminated it, and their edicts and justifications are the outside record of what the arrangement had become.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, meiji_separation_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how an imported Buddhist soteriology with state patronage and an indigenous kami cult with local rootedness and court ritual standing can occupy one religious field without permanent jurisdictional war: it assigns both a rank in a single cosmological order, lets shrines and temples share sites, personnel, revenues, and rites, and gives both traditions a common metaphysical language (original ground and trace) for coexistence.
% TRANSFER_FUNCTION: Moves doctrinal authority and institutional control from shrine lineages to Buddhist monastic corporations: kami are reclassified as provisional traces whose cult is valid under original-ground administration, channeling shrine revenues, ordination authority, and appointment power toward the temple complexes; in exchange shrines receive legitimation, protection, and integration into the national ritual economy.
% ABSENT_VOICES: Confucian scholars and nativist kokugaku thinkers objected for centuries — Confucians that the arrangement drained the realm toward foreign institutions, kokugaku scholars that honji-suijaku was a Buddhist colonization of the kami — but they stood outside the temple-shrine doctrinal establishment and held no seat in its councils until the political order that sustained the arrangement collapsed.
% DISAPPEARANCE_RATIONALE: When the Meiji state abolished the arrangement in 1868 the rearrangement was immediate and violent: shrine-temple complexes were forcibly divided, Buddhist priests expelled from shrines, thousands of temples destroyed and clergy laicized under haibutsu kishaku, and Shinto reconstructed as an independent national cult. A millennium of integrated practice, land tenure, and ritual calendar reorganized within a generation — arrangements across the society demonstrably depended on it.
% FOUNDING_PROBLEM: Heian religious life held two incommensurable cultic authorities: an imported Buddhist soteriology with scriptural sophistication and court patronage, and indigenous kami cults with local rootedness and ritual standing. Their coexistence generated recurring jurisdictional conflict — which authority governed a shared site, a mixed community, a court rite, a shrine's revenues?
% FOUNDING_PROBLEM_CORROBORATION: Heian court records and the Ryōbu and Sannō doctrinal scholia attest the original jurisdictional conflicts from inside the arrangement. From outside the benefiting parties, Meiji separation edicts and their nativist intellectual sponsors (the Motoori and Hirata lineages) attest that by the interval's end the arrangement persisted as a structure of Buddhist corporate interest rather than as a live answer to a coexistence problem; modern religious historiography (Kuroda Toshio's kenmitsu analysis) corroborates both the original coordination function and the later drift toward institutional rent.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base properties describe the arrangement's mature operating mode (roughly 1200-1650), not its terminal state; the measurement series runs the full lifecycle — emergence, medieval peak, Edo erosion, Meiji termination — on one shared time grid, with all three tracked metrics authored at every point. Extraction (0.62 mature) reflects the decoupling of Buddhist corporate benefit from service rendered to shrines: ordination, appointment, and revenue control rode on doctrinal precedence rather than on costs borne. Suppression (0.58) is structural and institutional — ordination requirements, appointment power, revenue dependency, doctrinal policing — rather than violently coercive for most of the interval; notably, the large-scale violence in the historical record runs the other way, with the Meiji state destroying the arrangement in 1868. Theater is low-to-moderate (0.20 mature) because the doctrinal and institutional apparatus did real integrative work; it rises late (0.46 by 1800) as maintenance becomes increasingly performative after conviction and intellectual monopoly erode. Accessibility collapse is moderate (0.5): alternatives never fully collapsed — reverse doctrines, independent shrine theologies, and Ise's periodic defiance persisted — but each paid a heavy legitimacy price. Resistance is substantial (0.5) and continuous: Watarai, Yoshida, kokugaku. The claim (tangled_rope) and the metrics are authored independently; the engine computes per-seat classifications from the structural data, and divergence between the claim and any computed seat is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently by construction. From the Buddhist monastic seat, the arrangement is a doctrinal achievement it authored and administers: kami-buddha identity is the truth of the matter, shrine integration its natural expression, and the revenues and appointments the just returns to the original ground. From the shrine-priesthood seat, the same structure operates as subordination: their kami are valid only as traces, their lineages legitimate only through Buddhist ordination, their revenues channeled through temple complexes. The lay community seat is near-symmetric — it receives genuine soteriological and festival benefits and pays through offerings and tithes, often without encountering the hierarchy at all (see the lay-experience omega). Among same-level institutional actors, the court and the warrior governments both hold powerful positions but experience the arrangement differently: the court's exit is constrained because its own ritual legitimacy is woven into the framework, while the warrior houses hold mobile exit — patronage was a purchase, redirectable at will, and the Tokugawa in practice let shrine-rank administration drift into reverse-doctrine hands. The excluded scholarly seats saw a structure the participants mostly did not name: Confucians saw parasitism, kokugaku scholars saw colonization.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist establishment is the structural beneficiary and agenda-setter: the original-ground position is the doctrine's apex, and revenues, appointments, and doctrinal authority flow there — its d sits near the beneficiary end, damped further by arbitrage-grade exit (the corporations adapted the framework to every new school and patron across nine centuries). Shrine priesthoods and independent Shinto lineages are the targets: they bear the trace position, and their exit is constrained by legitimacy dependency — d near the target end. Worshiping communities sit near symmetric: integration benefits and diffuse costs flow through the same shrine-temple complexes. The court and warrior governments are beneficiaries whose payments were purchases of legitimation and governance, not extraction borne — their d sits low. No directionality overrides are declared: the beneficiary/victim declarations plus exit options carry the derivation, and the dual-positioned seats (court, lay communities) are handled through secondary_role rather than override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents two mislabels. Reading the arrangement as pure extraction (a snare) erases the millennium of genuine coordination: it did solve the coexistence problem, did deliver real benefits to shrines, worshippers, and the court, and was not maintained by coercion alone — conviction, integration, and shared practice carried it for centuries. Reading it as pure coordination (a rope) erases the hierarchy: the same structure that integrated also ranked, and the ranking is where the asymmetric transfer lived. The mandatrophy arc is real but exogenously terminated: by the Edo period the founding problem (managing Buddhist-kami coexistence) had receded into routine, and the arrangement was increasingly maintained by institutional interest, inertia, and performative doctrinal maintenance — the R5 interview records the founding problem as dead at the interval's end while the world demonstrably rearranged when the arrangement was destroyed, the mismatch signature the capture/zombie consumer reads. The arrangement never declared its own sunset; the Meiji state imposed one from outside, which is why the terminal measurements describe a killed constraint rather than a completed internal transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the syncretic reading of the shinbutsu_ontological_commitment kernel; what would the partition and incoherence sibling readings change structurally if instantiated instead?',
    'Author the sibling stories and compare: partition_reading would remove the Buddhist-hierarchy-over-shrines structure (no ontological subordination of kami, victims set empties, ε drops sharply); incoherence_reading would dissolve the constraint entirely (no stable commitment, no parties organized around it, disappearance verdict world_unchanged).',
    'Under partition_reading the classification moves toward rope with minimal extraction; under incoherence_reading there is no constraint to classify at all — the corpus entry becomes a historian''s reification. The syncretic reading''s tangled_rope profile is conditional on its ontological premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three mutually exclusive readings of one kernel; siblings are separate constraints.').

omega_variable(
    scriptural_warrant_authenticity,
    'The syncretic arrangement claimed sutra-based warrant for kami-buddha identity; how much of that textual warrant was genuine transmission and how much constructed (apocryphal attribution, selective reading) to serve institutional integration?',
    'Philological analysis of the doctrinal corpus (Ryōbu and Sannō scholia, the sutra passages the doctrine cites) against Indian and Chinese antecedents, with dating of key texts.',
    'If the warrant is largely constructed, the authority structure is better read as extraction-grounded than lineage-grounded and theater_ratio rises; if genuine, the arrangement is doctrinal development rather than institutional fabrication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_warrant_authenticity, empirical, 'Whether the doctrine''s claimed textual grounding is authentic or constructed.').

omega_variable(
    extraction_integration_balance,
    'What share of the arrangement''s operation was genuine coordination (integration benefits to shrines, communities, and the court) versus asymmetric transfer (Buddhist corporate control of shrine revenues, appointments, and doctrine)?',
    'Economic history of jingūji and shrine-temple complex accounts; comparison of shrine endowment trajectories under the arrangement versus after the 1868 separation; Kuroda Toshio''s kenmitsu analysis set against shrine-side records.',
    'A dominant transfer share supports the tangled_rope-to-snare end of the profile; a dominant coordination share would move the classification toward rope with the hierarchy as overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_integration_balance, empirical, 'Balance of coordination benefit against hierarchical transfer in the arrangement''s operation.').

omega_variable(
    lay_experience_ambiguity,
    'Did ordinary worshippers experience the kami-buddha hierarchy, or was honji-suijaku a clerical superstructure over kami devotion that continued substantially unchanged at the village level?',
    'Village-level religious records, pilgrimage and festival practice studies, and the material culture of combined shrine-temple sites (mandala, honji-suijaku iconography, mixed rites).',
    'If lay experience was largely unchanged, the victim set narrows to shrine elites and the measured extraction overstates the constraint''s social reach; if the hierarchy reached lay practice, extraction was broad-based and the payer class is much larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_experience_ambiguity, empirical, 'Depth of the hierarchy''s reach into lay religious experience.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of Shinto autonomy structural (ordination requirements, appointment control, revenue dependency) or internalized (shrine lineages adopting Buddhist self-understanding as their own)?',
    'Post-separation trajectory: after 1868, did shrine lineages revert to independent self-understanding quickly (suppression was structural) or did Buddhist-inflected self-conception persist for generations (suppression was partly internalized)?',
    'If substantially internalized, effective suppression exceeded the structural measure and outlived the arrangement itself; if structural, the 1868 separation should have released shrine autonomy immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of shrine autonomy.').

omega_variable(
    terminal_cause_ambiguity,
    'Did the arrangement end by internal decay (erosion of enforcement capacity, loss of conviction, drift) or by exogenous force (the Meiji state''s separation edicts)?',
    'Counterfactual analysis of the 1860s: was the pre-1868 equilibrium still self-sustaining, or had enforcement capacity and conviction already fallen below the maintenance threshold?',
    'If exogenous, the terminal measurements describe a killed constraint rather than a decayed one, and lifecycle drift inference should not read the 1868 collapse as internal; if internal, the story is a completed mandatrophy arc with the Meiji state merely executing the already-dead arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_cause_ambiguity, empirical, 'Exogenous termination versus internal decay at the interval endpoint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1050, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1050, 0.12).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1350, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1350, 0.18).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.24).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1650, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1650, 0.34).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1800, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1800, 0.46).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.55).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.32).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1050, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1050, 0.44).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1350, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1350, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.66).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1650, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1650, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1800, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1050, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1050, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1350, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1350, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1650, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1650, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1800, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The kernel shinbutsu_ontological_commitment decomposes into three readings per the ε-invariance principle: each reading sees a different standing arrangement with a different ε and different beneficiary/victim structure. This file is the syncretic reading (high institutional integration, doctrinal coherence, Buddhist hierarchy benefiting, Shinto autonomy suppressed — hence the tangled_rope structural profile). The partition reading sees separate domains with minimal ontological extraction; the incoherence reading sees no stable constraint at all. A fourth structure — the Yoshida house's reverse doctrine (hanhon-hisuijaku: kami original, buddhas traces) — is a distinct constraint with its own ε, not a reading of this kernel; it appears here only as the payer-resister stakeholder independent_shinto_lineages. The historically dominant syncretic reading is upstream: it set the legitimacy conditions under which the siblings were articulated, and its 1868 destruction is what made the partition reading institutionally actionable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
