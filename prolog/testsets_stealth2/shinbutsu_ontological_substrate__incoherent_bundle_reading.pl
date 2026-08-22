% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: State-Enforced Kami-Buddha Fusion (Incoherent Bundle Reading)
 *   domain: religious/historical/political
 *
 * SUMMARY:
 *   This story instantiates the incoherent_bundle_reading of the
 *   shinbutsu_ontological_substrate kernel (see kernel_context for the
 *   sibling structure): the enforced kami-buddha fusion — shinbutsu shugo and
 *   its honji suijaku rationalization — as a standing arrangement from the
 *   ritsuryo integration of the kami cults (late eighth century) to the Meiji
 *   separation edicts (1868). On this reading the arrangement never
 *   constituted a unified commitment: the doctrinal literature was post-hoc
 *   rationalization of institutional accretion, and the arrangement's
 *   millennium of stability is explained by continuous state enforcement —
 *   edict, shrine-temple binding, and finally compulsory household
 *   registration — rather than by doctrinal coherence. Assessed by this
 *   reading's own lights, with the standing arrangement as referent, epsilon
 *   is high: village households bore doubled ritual and fee obligations,
 *   kami-side lineages had their cults subordinated, exclusivist alternatives
 *   were licensed or proscribed, and the combined establishment collected the
 *   fees, land, and registration income. The arrangement's terminal event is
 *   the reading's strongest evidence: when the state's will reversed in 1868,
 *   the fusion dissolved within months — the signature of an enforcement-held
 *   bundle, not a lived commitment.
 *
 * KEY AGENTS:
 *   - japanese_state: agenda-setter and principal beneficiary (institutional/arbitrage) — issued the binding edicts, collected legitimacy, census, and supervision, and reversed the whole arrangement by decree in 1868
 *   - temple_shrine_establishment: secondary beneficiary and receipt-seat (institutional/constrained) — collected festival, funerary, and registration income on tax-exempt land; bore state supervision and, at the end, the destruction of its Buddhist half
 *   - village_practitioners: primary target (powerless/trapped) — bore doubled ritual obligations and fees with no exit from registration
 *   - hereditary_shrine_lineages: secondary target (moderate/identity_locked) — kami-side clergy whose cults and offices were subordinated inside the fusion
 *   - exclusive_kami_worship_movements: suppressed alternative (moderate/trapped) — defined against the fusion and licensed or proscribed by it
 *   - shamanic_practitioners: excluded popular ritualists (powerless/constrained) — served the same communities, held no seat
 *   - critical_historians: analytical observer — sees the full structure from institutional documents (Kuroda school)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.74).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.85).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "State-Enforced Kami-Buddha Fusion (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/historical/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '268cdb0b-52b7-4f26-a424-84b8b0d5deee').
narrative_ontology:cs_kernel_codification('268cdb0b-52b7-4f26-a424-84b8b0d5deee', implicit).
narrative_ontology:cs_authority_grounding('268cdb0b-52b7-4f26-a424-84b8b0d5deee', extraction).
narrative_ontology:cs_interpretation_layer_present('268cdb0b-52b7-4f26-a424-84b8b0d5deee').
narrative_ontology:cs_reading_relation('268cdb0b-52b7-4f26-a424-84b8b0d5deee', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('268cdb0b-52b7-4f26-a424-84b8b0d5deee', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('268cdb0b-52b7-4f26-a424-84b8b0d5deee', foundational, syncretism_is_institutional_accretion).
narrative_ontology:cs_axiom_status(syncretism_is_institutional_accretion, holdable).
narrative_ontology:cs_axiom_grounding('268cdb0b-52b7-4f26-a424-84b8b0d5deee', syncretism_is_institutional_accretion, empirically_contingent).
narrative_ontology:cs_axiom('268cdb0b-52b7-4f26-a424-84b8b0d5deee', foundational, state_enforcement_sustained_arrangement).
narrative_ontology:cs_axiom_status(state_enforcement_sustained_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('268cdb0b-52b7-4f26-a424-84b8b0d5deee', state_enforcement_sustained_arrangement, empirically_contingent).
narrative_ontology:cs_reference_frame('268cdb0b-52b7-4f26-a424-84b8b0d5deee', enforced_institutional_arrangement).
narrative_ontology:cs_drift_state('268cdb0b-52b7-4f26-a424-84b8b0d5deee', meiji_shinbutsu_bunri, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('268cdb0b-52b7-4f26-a424-84b8b0d5deee', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, japanese_state).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_establishment).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, exclusive_kami_worship_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central state apparatus across the interval — the ritsuryo court, then the warrior governments, then the Tokugawa bakufu — issued the edicts binding shrines to temples, mandated the combined ritual calendar, ranked the shrines, and from the seventeenth century required every household to register with a temple. It received legitimacy, a census, a supervised clergy, and an anti-Christian screening device in exchange, and it paid the administrative cost of running the machinery. In 1868 it reversed the entire arrangement by decree within months, which showed it had never surrendered the pen.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, japanese_state, agenda_setter,
    institutional, generational, arbitrage, national).

% The combined complex of head temples with their shrine branches, and great shrines with their resident temples, held tax-exempt land, collected festival, funerary, and registration fees from the households registered to it, and supplied the court's ritual needs. Its scholastic houses produced the doctrinal literature that rationalized the combined arrangement. It could not drop its shrine or its buddha without forfeiting official standing; it bore state supervision throughout and, at the end of the interval, the destruction of its Buddhist half under the separation edicts.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_establishment, payer).

% Farming households maintained both the kami festival cycle of their local shrine and the funerary obligations of their registered temple, paid fees to both sides, and could not register elsewhere, skip the festivals, or bury their dead outside the system. Villages ran the festivals through hereditary guilds; moving away or refusing meant losing registration, burial, and communal standing. After 1868 most of the same households shed the Buddhist obligations within a generation, and many quietly recombined shrine and temple elements once supervision relaxed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_practitioners, payer,
    powerless, generational, trapped, local).

% Hereditary priestly families — court lines such as the Nakatomi and O, and the local shrine families — saw their kami reclassified as manifestations of buddhas and their rites given Buddhist framing; at the great shrines, Buddhist clerics were installed over them as administrators. Their office, family name, and marriage alliances were the lineage's substance, so remaining inside the arrangement was the only way the family continued to be what it was. Some lines, notably the Yoshida, built counter-doctrines claiming kami priority from within the arrangement itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_lineages, payer,
    moderate, generational, identity_locked, regional).

% Movements asserting kami supremacy or exclusivity — the Yoshida school's claim that the buddhas are traces of the kami, Ise pilgrimage confraternities, the National Learning scholars — defined their entire program against the combined arrangement and were licensed, monitored, or suppressed by it. Their existence had no form apart from the arrangement they opposed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, exclusive_kami_worship_movements, payer,
    moderate, biographical, trapped, national).

% Popular ritualists outside both institutions — mediums, itinerant ascetics, unaffiliated mountain practitioners — served the same communities the combined establishment drew fees from, but held no seat in the court offices or clerical councils that decided what kami-buddha relations meant, and were periodically proscribed as unlicensed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shamanic_practitioners, excluded,
    powerless, biographical, constrained, local).

% Modern historians of Japanese religion, above all the Kuroda Toshio school, reconstructed the arrangement from institutional documents — land records, edicts, registration returns — rather than from the doctrinal self-descriptions, and produced the account this story instantiates. They hold no position inside the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, critical_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solved a real governance problem: it mapped thousands of local kami cults onto a single administrable ritual grid. A unified festival calendar, shrine ranking, and — from the Tokugawa period — household registration through temples gave the state a census, a funerary monopoly, and a supervised clergy; local communities received recognized standing for their cults within the state's order.
% TRANSFER_FUNCTION: Moved ritual labor, festival funding, and — from the Tokugawa period — funerary and registration fees from village households to the temple-shrine complex; moved tax-exempt land, jurisdiction, and administrative services to the combined clergy; moved legitimacy, a census, and ideological supervision to the state.
% ABSENT_VOICES: The practitioners whose beliefs the doctrine described were never seated: what kami-buddha relations meant was adjudicated by court offices, scholastic houses, and state councils. Kami-side ritualists whose cults were absorbed had voice only through lineages that had already accepted the fusion's terms. Unaffiliated popular ritualists were outside the conversation entirely and periodically proscribed. Exclusivist movements were handled as licensing problems, not interlocutors.
% DISAPPEARANCE_RATIONALE: It did not have to be hypothesized: when the state reversed its will in 1868, the arrangement vanished within months — edicts severed shrines from temples, thousands of Buddhist institutions were closed or destroyed, registered households shed their temple obligations, and the religious map of the country reorganized around a state shrine system within a decade. A roughly millennium-old arrangement that dissolves on a change of administrative will was held by that will.
% FOUNDING_PROBLEM: The ritsuryo state, building a centralized government on Tang models, needed to integrate the archipelago's thousands of local kami cults into one ritual-political order; the fusion answered by reading local kami as local manifestations of the universal Buddhist framework the state had adopted.
% FOUNDING_PROBLEM_CORROBORATION: No party that benefited from the arrangement attests that its founding problem remained live, so corroboration comes from outside the beneficiary set: the documentary record shows the ritsuryo system's collapse by the tenth century; the Meiji state's own separation edicts described the fusion as a medieval accretion to be corrected rather than a living solution to a current problem; and exclusivist movements from Nichiren through the National Learning scholars testified from within the era that the arrangement answered no coherent doctrinal question. Kuroda-school historiography corroborates from the analytical seat.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rises with each enforcement layer: court-mandated shrine-temple integration and combined rites (0.40 at interval start), medieval temple-shrine estates and jurisdiction (0.48 to 0.60), and the Tokugawa terauke system's compulsory household registration with its funerary fee monopoly (0.70), reaching 0.74 at the 1868 endpoint. Suppression is authored as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled, engine-side, by directionality and scope. The arrangement was never self-sustaining: it required continuous edict, licensing, and proscription (Christianity banned, exclusivist movements monitored, unaffiliated ritualists periodically criminalized), peaking at 0.85 under the Tokugawa registration apparatus; the terminal point records the enforcement machinery at peak capacity at the moment it was seized and reversed. Theater rises from 0.18 to 0.62: early integration was administrative, but the scholastic apparatus (ryobu Shinto, Sanno Shinto, the honji suijaku treatises, the Yoshida counter-system) grew ever more elaborate as rationalization of institutionally settled facts — performance covering the arrangement rather than generating it. Accessibility_collapse is moderate (0.55) because alternatives never fully collapsed: exclusivist movements persisted under license and folk practice quietly recombined elements throughout. Resistance is moderate (0.5) and episodic rather than sustained: Nichiren's exclusivity, the Yoshida reverse-hierarchy, village fee and land disputes, and post-1868 riots defending temples. Coalition potential among the powerless was real but bounded — villages organized collectively through miyaza guilds and mounted occasional fee and land uprisings, yet the fusion was administered through the villages' own ritual life, so collective action targeted particular temples and exactions rather than the arrangement itself. The three measurement series run on one shared grid (794/1000/1200/1400/1600/1868) with every tracked metric authored at every point; the trajectories are monotonic ratchets, not cycles, so the cyclical-measurement apparatus for interpersonal constraints does not apply.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the state seat the arrangement was a governance instrument it held the pen on — 1868 proved the pen was never surrendered. From the village seat it was an inherited double obligation with no individual exit and only bounded collective recourse. From the establishment seat it was a rent structure it collected under but did not control, and lost wholesale when the pen moved. The hereditary shrine lineages are the identity-locked case: the lock is institutional and relational at once — the lineage's ritual office, family name, and marriage alliances were the family's substance, so from inside the fusion looked like tradition rather than imposition, and exit meant dissolving what the family was. Had that identity frame broken earlier (as the Yoshida counter-doctrine attempted from inside), the lineage seat's position would have moved toward the exclusivist seat's. The engine computes these divergences from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put the state and the combined establishment at the low-d end; the establishment's secondary payer position (state supervision throughout, terminal destruction of its Buddhist half) moderates its d below a pure beneficiary's. Victim declarations put village practitioners, shrine lineages, and exclusivist movements at the high-d end, with exit atoms doing the amplification: trapped registration for villages, identity_locked office for lineages, trapped oppositional identity for the exclusivists. The excluded shamanic practitioners sit at high d without any formal role in the arrangement. The analytical observer seat is handled as such. The beneficiary/victim declarations plus the exit atoms produce the correct relationships, so no directionality_overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating thousands of local kami cults into a centralized Tang-modeled ritual state — died with the ritsuryo system by the tenth century; the arrangement persisted roughly nine more centuries under warrior and Tokugawa enforcement serving successor purposes (jurisdiction, registration, revenue, anti-Christian supervision). Authoring founding_problem_status 'dead' against disappearance_verdict 'world_rearranges' is the honest mismatch: it flags the capture/zombie pattern for the consumer, cross-checked against the computed theater path. The snare claim is what prevents mislabeling: the arrangement's coordination story (ritual unity, a national cult) is cover for a mechanism that operated by compulsory registration and proscription — and the 1868 reversal is the terminal experiment separating an enforcement-held arrangement from a lived commitment. A genuine rope would have survived its enforcer's change of mind; this did not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which account of the arrangement''s millennium of stability is correct — doctrinal coherence (syncretic_fusion_reading), functional division of labor (domain_partition_reading), or enforcement alone (this reading)?',
    'Partly framing-dependent: the sibling readings are separate constraints with separate epsilon values, so resolution first requires agreement on what counts as a coherent kernel (doctrinal assent, functional fit, or explicit commitment), and only then weighing the documentary record of edicts, registration returns, and the doctrinal corpus against that standard.',
    'Adopting the fusion reading would re-author this arrangement''s epsilon near zero and its type toward rope; adopting the partition reading would re-author it as moderate-extraction coordination; this story''s high-epsilon snare assessment stands only under the no-coherent-kernel premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one of three rival readings of the shinbutsu_ontological_substrate kernel; the siblings live in separate constraint files.').

omega_variable(
    assent_vs_compliance,
    'How much genuine assent did the fused cosmology command versus compliance under compulsory registration — was the suppression practitioners experienced structural, internalized, or both?',
    'Post-1868 trajectory: the speed and completeness of the rearrangement once enforcement flipped (temple closures and destruction within years; danka obligations shed within a generation in most regions), weighed against the counter-evidence of regional riots defending temples and the quiet re-fusion of shrine and Buddhist elements after early-Meiji fervor cooled.',
    'Substantial internalized assent would mean this story''s suppression figure overstates structural coercion and the arrangement carried rope-like elements this story does not credit; compliance dominance confirms the snare assessment and makes the 1868 collapse the natural experiment that proves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assent_vs_compliance, empirical, 'Structural versus internalized component of practitioner compliance with the fused arrangement.').

omega_variable(
    enforcer_benefit_continuity,
    'Did each successor regime (ritsuryo court, warrior governments, Tokugawa bakufu) still benefit from enforcing the fusion, or did enforcement persist as bureaucratic inertia after the founding beneficiaries vanished?',
    'Regime-by-regime comparison of enforcement intensity against documented benefit: ritsuryo ritual integration records, medieval land and jurisdiction grants to temple-shrine complexes, Tokugawa registration returns and anti-Christian enforcement files.',
    'If late-period enforcement served no beneficiary, the arrangement''s later centuries drift toward the piton profile (inertial maintenance without a capturer) and the single snare arc decomposes into a snare phase followed by an inertial phase; if benefit persisted, the snare reading holds across the full interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcer_benefit_continuity, empirical, 'Whether enforcement tracked continuing state benefit or outlived it as inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t794, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 794, 0.18).
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t1000, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1400, 0.48).
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1600, 0.55).
narrative_ontology:measurement(shinbutsu_bundle_reading_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.62).

% Extraction over time
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t794, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 794, 0.4).
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t1000, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1000, 0.48).
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1400, 0.6).
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(shinbutsu_bundle_reading_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t794, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 794, 0.45).
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t1000, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1400, 0.68).
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(shinbutsu_bundle_reading_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu syncretism' covers three structurally distinct claims about the same standing arrangement, per the epsilon-invariance principle: syncretic_fusion_reading (the arrangement expresses ontological unity; epsilon assessed low by its lights), domain_partition_reading (the arrangement embodies a coherent functional division of labor; epsilon moderate), and this story, incoherent_bundle_reading (the arrangement is an enforcement-held institutional bundle with no coherent kernel; epsilon high). One referent, three readings, three epsilon values, three constraint files; each links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
