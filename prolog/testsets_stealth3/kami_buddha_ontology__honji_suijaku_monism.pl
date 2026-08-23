% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Hierarchical Monism (Kami as Buddha Traces)
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   Honji suijaku ('original ground, manifested traces') is the medieval
 *   Japanese doctrinal settlement under which every major kami was paired
 *   with a buddha or bodhisattva understood as that deity's original ground:
 *   Amaterasu with Dainichi in the Ryobu system, Hachiman with Amida, and
 *   onward through systematic correspondence tables authored and policed by
 *   the great esoteric academies. The arrangement solved a real problem, two
 *   rival salvific economies sharing one archipelago, while embedding an
 *   asymmetric hierarchy: shrine institutions received court patronage and
 *   protection only by accepting trace-status for their deities, and Buddhist
 *   complexes collected ritual headship, chapel sitings, and shares of shrine
 *   income through the same structure. This file instantiates ONE reading of
 *   the contested kami-buddha-ontology kernel, the monist reading; sibling
 *   readings (functional domain partition, and the no-coherent-kernel thesis)
 *   are separate constraints in separate files, and nothing here hedges or
 *   averages across them. The epsilon referent is the standing arrangement
 *   under contest, the honji suijaku regime as it actually operated, assessed
 *   by this reading's own lights, which register genuine integration
 *   alongside admitted hierarchy; the authored extractiveness is
 *   correspondingly moderate, and a hostile reading of the same referent
 *   would author a higher value. The claim and the metrics are independent
 *   authored facts: claimed_type records my structural judgment (real
 *   coordination, real asymmetry, active enforcement), and the metrics record
 *   what the monist seat takes to be descriptively true. Interval mapping:
 *   one time unit equals one century, t0 approximately 900 CE
 *   (crystallization of the Ryobu and Sanno systematizations) to t10
 *   approximately 1900 CE, after the Meiji separation edicts dissolved the
 *   arrangement; the terminal measurements report forced-dissolution residue
 *   rather than steady state, so the base_properties scalars describe the
 *   mature phase (roughly t2 through t8) rather than the endpoint.
 *
 * KEY AGENTS:
 *   - - esoteric_buddhist_complexes: agenda-setting doctrinal author (institutional/arbitrage) — writes and polices the correspondence tables; collects headship, chapel sitings, and offering shares
 *   - - great_shrine_priesthoods: dual-positioned cult elites (organized/constrained) — accept subordination in exchange for patronage and protection; net ledger modestly loss-bearing
 *   - - local_kami_cult_communities: diffuse worshipper base (powerless/trapped) — bear rerouted festivals, monopolized funerals, and treasury routing with no mobility off registered land
 *   - - court_patron_networks: patron-consumers (powerful/mobile) — fund both systems and consume the unified ritual calendar
 *   - - kokugaku_scholars: late-arriving dissidents (moderate/mobile) — construct the rival kami-first ontology outside the establishment
 *   - - historians_of_japanese_religion: analytical observers — reconstruct the structure from documents without exposure to its flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.46).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.55).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.46).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Hierarchical Monism (Kami as Buddha Traces)").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '67dc30a1-c4f2-4365-9cf7-e63c4230d78f').
narrative_ontology:cs_kernel_codification('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', formalized).
narrative_ontology:cs_authority_grounding('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', lineage).
narrative_ontology:cs_interpretation_layer_present('67dc30a1-c4f2-4365-9cf7-e63c4230d78f').
narrative_ontology:cs_reading_relation('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', foundational, kami_have_no_independent_existence).
narrative_ontology:cs_axiom_status(kami_have_no_independent_existence, holdable).
narrative_ontology:cs_axiom_grounding('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', kami_have_no_independent_existence, theological).
narrative_ontology:cs_axiom('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', secondary, kami_cult_requires_buddhist_completion).
narrative_ontology:cs_axiom_status(kami_cult_requires_buddhist_completion, holdable).
narrative_ontology:cs_axiom_grounding('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', kami_cult_requires_buddhist_completion, instrumental).
narrative_ontology:cs_reference_frame('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', buddha_ground_trace_hierarchy).
narrative_ontology:cs_drift_state('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', late_edo_nativist_ascendancy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('67dc30a1-c4f2-4365-9cf7-e63c4230d78f', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_complexes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, court_patron_networks).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, great_shrine_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_kami_cult_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, great_shrine_priesthoods).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, dharmakaya_priority).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, expedient_manifestation_upaya).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the great doctrinal academies (Tendai on Mount Hiei, Shingon on Koyasan) that author the correspondence tables pairing each major deity with a buddha or bodhisattva and police adherence to them. Register shrines as branch institutions, establish chapels on shrine grounds, collect headship fees and shares of offerings, and conduct the rites the doctrine holds to complete what kami worship lacks on its own. Their assets span teaching, landholding, and ritual office, and their position remains sound under any ontology they themselves endorse.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_complexes, agenda_setter,
    institutional, generational, arbitrage, national).

% Hereditary custodians of the oldest cult centers (Ise, Izumo, Kamo, and their peer lineages). Accepting the correspondence tables brought imperial patronage, festival financing, and relief from open absorption; declining brought lawsuits, economic pressure, and occasionally armed visitation. Lineages such as Ise, with its recurring exclusivity claims, and Izumo, with its succession disputes, spent centuries negotiating the terms of their own subordination. Materially the great houses prospered; their deities' rank fell and their jurisdictional independence narrowed.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, great_shrine_priesthoods, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, great_shrine_priesthoods, beneficiary).

% Village worshippers whose shrines were folded into combined temple-shrine complexes: festivals moved onto Buddhist calendars, offerings routed through complex treasuries, funerals and memorial rites monopolized by resident clergy. Tenancy and parish registration tied households to the land, so their ritual lives followed whatever registration their lords arranged; leaving meant abandoning graves and kin networks.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_kami_cult_communities, payer,
    powerless, biographical, trapped, local).

% Fund both halves of the settlement from provincial tax assignments and consume its output: a single calendar of state rites covering harvests, ancestors, and cosmic order without adjudicating between the two systems. Patronage is discretionary, and funds shifted repeatedly between temples and shrines, which keeps their exposure light and their options open.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, court_patron_networks, beneficiary,
    powerful, biographical, mobile, national).

% Edo-period scholars of National Learning (Keichu, Kada no Azumamaru, Motoori Norinaga, Hirata Atsutane) who reconstructed a kami-first antiquity from the earliest texts and rejected the trace-doctrine root and branch. They published, taught, and gathered domain sponsorship outside the established clergy; their reading became state policy almost immediately after the interval closes.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kokugaku_scholars, excluded,
    moderate, biographical, mobile, national).

% Modern scholars of Japanese religion who reconstruct the arrangement from origin legends (engi), ritual registers, estate documents, and doctrinal commentaries, and compare the monist reading against its rivals. They hold no position inside the exchange and bear none of its flows.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, historians_of_japanese_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, esoteric_buddhist_complexes).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified two rival salvific economies, the temple networks and the kami shrine networks, into one liturgical order: shared festival calendars, cross-registered clergy, combined temple-shrine complexes administering both, and a single doctrinal account that let patrons fund both systems without adjudicating a winner.
% TRANSFER_FUNCTION: Moved doctrinal authority, ritual headship, and a substantial share of shrine income upward from shrine institutions to Buddhist complexes; moved legitimation downward to shrine lineages conditional on accepting trace-status for their deities; moved festival labor and offerings from rural worshipping communities into the fused complexes.
% ABSENT_VOICES: Independent kami theologians had no seat in the councils that codified the correspondence tables; shrine lineages that resisted registration, notably Ise's recurring exclusivity claims, negotiated from weakness; rural worshippers were represented only through their lords. Organized dissent acquired a voice only with Edo-period National Learning, near the interval's end.
% DISAPPEARANCE_RATIONALE: The Meiji separation edicts (1868-71) and the accompanying wave of anti-Buddhist demolition demonstrate it: shrines were stripped of Buddhist accoutrements, thousands of combined complexes dissolved, clergy were forcibly reassigned to Shinto or Buddhist categories, and the state built Imperial Shinto on the vacated ground. The ritual economy reorganized within a generation.
% FOUNDING_PROBLEM: In the Nara-Heian transition, an imported Buddhist establishment with superior literate technology and state favor threatened to absorb or marginalize the native kami cults, while the court needed one legitimate ritual order covering both its tax base and its sacred geography.
% FOUNDING_PROBLEM_CORROBORATION: The Buddhist complexes attest the founding problem stayed live, citing the continuing need for doctrinal completion of kami cults. Against them, Edo-period National Learning scholars, Neo-Confucian advisers in several domains, and shrine petitioners seeking release from temple headship all attest, from outside the beneficiary set, that coexistence had been secured centuries earlier and the hierarchy persisted as institutional habit; the Meiji state acted on that reading. During the medieval peak itself no corroborator outside the beneficiary set endorsed either verdict, and that silence in the record is itself signal.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).
:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the transfer is real (headship fees, offering shares, chapel sitings, calendrical control) but the monist seat prices it as the operating cost of a hierarchy it regards as ontologically truthful rather than as naked rent; a partition or bundle reading of the same referent would author higher. Suppression 0.55: enforcement ran through economic dependency, with shrine finances routed through complex treasuries, and doctrinal policing rather than mass coercion, and much compliance was sincere conviction; the conviction-versus-enforcement split is carried as an omega rather than forced into the scalar. Theater_ratio 0.28: the systematization performed real integrative work for roughly eight centuries (shared calendars, cross-registration, dispute resolution); performative maintenance dominates only in the Tokugawa tail. Accessibility_collapse 0.62: within elite doctrinal discourse the monist frame closed off independent kami-theology almost completely for about seven hundred years, but folk dualism persisted underneath and Nativist scholarship reopened the space at the interval's end, so alternatives collapsed substantially without vanishing. Resistance 0.45: episodic shrine litigation, refusals of registration, and late organized Nativist revolt; never sustained mass resistance before the state-led termination. All three tracked metrics run on one shared grid (t = 0,2,4,6,8,9,10) so no row substitutes an end-state value into earlier centuries. The trajectory is monotonic rise, plateau, decay, then forced collapse rather than oscillation, so no intermittent-reinforcement reading applies. Suppression_requirement is tracked because the enforcement picture is genuinely dynamic: the chapel-and-treasury ratchet builds through the Muromachi peak, then decays after the state turns against the arrangement in 1868-71.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the esoteric complexes' seat the arrangement is revelation management they authored: a single ground expressing itself in two registers, with the fees and headship as administrative overhead, so their computed type should come out coordination-heavy. From the great shrine seat it is a contingent partnership that hardened into subordination: real protection and patronage received, deity-rank and jurisdiction surrendered, a genuinely mixed computation. From the village seat it is administered extraction with no exit: festivals rerouted, funerals monopolized, no mobility off registered land, the most target-heavy computation available. The court seat consumes the output, one legitimate ritual calendar, and pays willingly; the Nativist seat reads the whole structure as usurpation. The engine derives these divergences from power, exit, and declared position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. esoteric_buddhist_complexes: named beneficiary, controls the rules, arbitrage-grade exit, sits nearest the beneficiary end (d approximately 0.08), so effective extraction damps toward subsidy for them. court_patron_networks: named beneficiary with discretionary patronage and mobile exit, low-moderate d (approximately 0.25). local_kami_cult_communities: named victim, trapped by tenancy and parish registration, near-full target (d approximately 0.85), further amplified by verification difficulty at scale. great_shrine_priesthoods: dual-positioned, listed as payer with beneficiary as secondary role; the automatic derivation cannot split a dual role cleanly, so a directionality override sets the organized seat to d = 0.62, net target-side, because the material gains were conditional on accepting subordination and the condition outlasted the gains. kokugaku_scholars stand outside the transfer loop as the excluded seat, and the historian seat is analytical with no flow. Extractiveness is scaled by directionality and spatial scope in the engine's arithmetic; suppression is authored raw and unscaled, and this commentary treats it accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled_rope keeps two errors apart. Calling this a snare would erase the genuine coordination: shrine elites entered the settlement voluntarily, drew real protection and financing, and the fused order delivered eight centuries of shared ritual infrastructure neither system could have built alone. Calling it a rope would erase the asymmetry: the ontological terms were authored unilaterally by the complexes, enforced through financial dependency, and priced in deity-rank; someone was coordinated and someone paid through the same structure, which is precisely the hybrid signature. On obsolescence: the founding problem, two rival salvific economies colliding, was substantially solved by the Kamakura period, yet the hierarchy ran roughly five more centuries on institutional momentum, a classic mandate-outlived-function profile. It never fully atrophied into pure performance because the fused institutions kept delivering real services, festivals, financing, dispute resolution, so the theater_ratio climb in the Tokugawa tail is an atrophy signature arriving just before external termination; the arrangement was killed by state edict in 1868-71 while still partially functional, which is why the record shows decay-plus-collapse rather than completed pitonization. Because the founding-problem status is genuinely contested rather than plainly dead, the story leaves the mismatch consumer to read contested status against the world_rearranges verdict rather than asserting a zombie flag itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates only the honji_suijaku_monism reading of the kami_buddha_ontology kernel; what would the sibling readings measure, and where exactly does the disagreement bite?',
    'Compare the three sibling stories'' epsilon values, victim sets, and computed classifications. The divergence is localized at one structural element: whether kami possess independent existence. The domain_partition reading affirms independent jurisdiction and shifts the cost ledger toward whichever side loses autonomy; the incoherent_bundle reading denies any stable single epsilon and forces seat-by-seat bundle analysis.',
    'Under domain_partition the arrangement''s classification hinges on jurisdictional winners and losers rather than on trace-subordination; under incoherent_bundle no story-level type is stable and this file''s tangled_rope verdict holds only within the monist reading. Cross-reading comparison is the designed consumption path, not a defect of this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    shrine_elite_net_position,
    'Were the great shrine priesthoods net beneficiaries (court patronage, festival financing, protection from absorption) or net payers (loss of deity-rank and jurisdictional autonomy) across the interval?',
    'Ledger reconstruction of shrine incomes before and after registration under combined complexes, plus petition records where shrine lineages sought exemption from temple headship (Ise exclusivity claims, Izumo succession disputes).',
    'If net-beneficiary, the extraction asymmetry narrows toward a coordination-dominant verdict and the organized-seat directionality falls; if net-payer, the tangled_rope reading strengthens and the override value of 0.62 rises toward the trapped-seat range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shrine_elite_net_position, empirical, 'Net ledger of the dual-positioned shrine elite seat.').

omega_variable(
    conviction_vs_enforcement_split,
    'How much of the arrangement''s stability rested on sincere monist conviction versus enforced compliance (financial dependency of shrines on complex treasuries, doctrinal policing)?',
    'Post-separation behavior: after 1868 removed enforcement at modest popular cost, former combined-complex congregations largely accepted separation without sustained restoration movements, suggesting enforcement carried more weight than the scalar implies; conversely, surviving honji suijaku devotion in folk practice suggests durable conviction. Weigh both records.',
    'If enforcement-dominated, suppression is understated and effective extraction runs higher than authored; if conviction-dominated, the arrangement approaches voluntary coordination and the rope component dominates the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_enforcement_split, empirical, 'Split of the suppression scalar into structural versus conviction-carried components.').

omega_variable(
    counterfactual_coexistence_loadbearing,
    'Could the two religious economies have coexisted without the monist hierarchy, i.e., is the coordination function attributable to the ontology or to court mediation that would have sufficed anyway?',
    'Comparative cases: Korean Buddhist-indigenous accommodation and Chinese daoist-buddhist modus vivendi achieved coexistence under different metaphysics; assess whether Japan''s specific hierarchy was load-bearing or decorative over a mediated peace.',
    'If court mediation suffices, the ontology is superstructure over a coordination substrate and its measured costs are largely parasitic; if the ontology was load-bearing, removing it courts the zero-sum conflict it prevented, which raises the cost term in any assessment of the Meiji termination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_coexistence_loadbearing, conceptual, 'Counterfactual necessity of the hierarchical frame for the coordination it delivered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(kami_tr_t0, observed).
narrative_ontology:measurement(kami_tr_t2, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 2, 0.15).
narrative_ontology:measurement_basis(kami_tr_t2, observed).
narrative_ontology:measurement(kami_tr_t4, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(kami_tr_t4, observed).
narrative_ontology:measurement(kami_tr_t6, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(kami_tr_t6, observed).
narrative_ontology:measurement(kami_tr_t8, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(kami_tr_t8, observed).
narrative_ontology:measurement(kami_tr_t9, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 9, 0.52).
narrative_ontology:measurement_basis(kami_tr_t9, observed).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 10, 0.78).
narrative_ontology:measurement_basis(kami_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(kami_be_t0, observed).
narrative_ontology:measurement(kami_be_t2, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(kami_be_t2, observed).
narrative_ontology:measurement(kami_be_t4, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(kami_be_t4, observed).
narrative_ontology:measurement(kami_be_t6, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(kami_be_t6, observed).
narrative_ontology:measurement(kami_be_t8, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(kami_be_t8, observed).
narrative_ontology:measurement(kami_be_t9, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 9, 0.47).
narrative_ontology:measurement_basis(kami_be_t9, observed).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 10, 0.07).
narrative_ontology:measurement_basis(kami_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(kami_su_t0, observed).
narrative_ontology:measurement(kami_su_t2, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 2, 0.5).
narrative_ontology:measurement_basis(kami_su_t2, observed).
narrative_ontology:measurement(kami_su_t4, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(kami_su_t4, observed).
narrative_ontology:measurement(kami_su_t6, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 6, 0.63).
narrative_ontology:measurement_basis(kami_su_t6, observed).
narrative_ontology:measurement(kami_su_t8, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(kami_su_t8, observed).
narrative_ontology:measurement(kami_su_t9, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 9, 0.54).
narrative_ontology:measurement_basis(kami_su_t9, observed).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 10, 0.05).
narrative_ontology:measurement_basis(kami_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' (kami-buddha syncretism) conflates three structurally distinct claims with different epsilon profiles and victim sets: hierarchical monism (this file), functional dualism (kami_buddha_ontology__domain_partition), and the no-coherent-kernel thesis (kami_buddha_ontology__incoherent_bundle). Per the epsilon-invariance principle each is authored separately and linked here. Pressure runs from the monist reading outward: it supplied the doctrinal machinery (correspondence tables, chapel registration) that both siblings define themselves against, so its operation shapes the environment in which the partition reading survives as folk practice and the bundle thesis finds its evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
