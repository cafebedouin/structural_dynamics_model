% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Caste-Gated Devotional Order as Contested by the Universalist Bhakti Reading of the Gita
 *   domain: religious/textual-hermeneutic/social
 *
 * SUMMARY:
 *   This story instantiates the universalist_devotional_reading of the
 *   gita_kurukshetra_discourse kernel: the claim that the Gita teaches a
 *   path-independent devotion (bhakti) open to all regardless of caste, and
 *   that dharma is surrender to divine will rather than social role. Per the
 *   ε-referent rule for kernel-reading stories, the story is ABOUT the
 *   standing arrangement under contest — the caste-gated devotional order
 *   that administers study, rite, and temple entry under the text's authority
 *   — and ε is authored for that arrangement as the universalist reading sees
 *   it: a ritual economy that genuinely coordinates religious life across the
 *   subcontinent while rationing spiritual access by birth and concentrating
 *   the gains that rationing makes possible. The reading's endorsed
 *   alternative (open-access devotion; the universal devotee class as its
 *   beneficiary) is NOT the referent and does not appear in the beneficiary
 *   declarations; it appears instead as the organized contest inside the
 *   order (cross_caste_bhakti_communities) and in the omega variables. Claim
 *   and metrics are authored independently: claimed_type is tangled_rope
 *   because the same structure that coordinates ritual life also concentrates
 *   its gains in one seat and actively enforces the gate; the metric values
 *   describe the order's operation as this reading assesses it. The
 *   divergence a per-seat computation will show between the priestly seat and
 *   the excluded seats is the measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: agenda-setter and primary beneficiary (institutional / identity_locked) — administers the gate on study, rite, and temple entry; collects dakshina, land, and deference; its status and self-concept are constituted by the order it runs
 *   - twice_born_patron_elites: secondary beneficiary (powerful / constrained) — purchase rank-legitimation through patronage; can shift patronage across sects but their varna standing is minted only inside this order
 *   - shudra_devotees: primary target (powerless / trapped) — barred from Vedic study, receive rite only through paid mediation
 *   - outcaste_communities: primary target (powerless / trapped) — excluded from temple and rite entirely; bear the pollution burdens
 *   - women_of_devout_households: target (powerless / constrained) — barred from Vedic recitation; licensed channel is household devotion and vernacular song
 *   - cross_caste_bhakti_communities: organized contesting payers (organized / constrained) — demonstrate open devotion from inside the order, paying sanction costs while drawing warrant from the canon itself
 *   - vernacular_saint_poets: excluded voice (moderate / identity_locked) — spoke the universalist claim from outside the gate; marginalized or domesticated
 *   - constitutional_reform_state: second agenda-setter (institutional / constrained) — administers the erosion: temple-entry statutes, constitutional equality
 *   - textual_scholarship: analytical observer (analytical / analytical) — sees the full structure: strata, reading contest, rent flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.74).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.6).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Caste-Gated Devotional Order as Contested by the Universalist Bhakti Reading of the Gita").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious/textual-hermeneutic/social").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'aa5f54b8-254a-4cd3-9944-b2403e98b72b').
narrative_ontology:cs_kernel_codification('aa5f54b8-254a-4cd3-9944-b2403e98b72b', fixed_text).
narrative_ontology:cs_authority_grounding('aa5f54b8-254a-4cd3-9944-b2403e98b72b', extraction).
narrative_ontology:cs_interpretation_layer_present('aa5f54b8-254a-4cd3-9944-b2403e98b72b').
narrative_ontology:cs_reading_relation('aa5f54b8-254a-4cd3-9944-b2403e98b72b', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa5f54b8-254a-4cd3-9944-b2403e98b72b', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_axiom('aa5f54b8-254a-4cd3-9944-b2403e98b72b', foundational, devotion_dissolves_birth_barriers).
narrative_ontology:cs_axiom_status(devotion_dissolves_birth_barriers, holdable).
narrative_ontology:cs_axiom_grounding('aa5f54b8-254a-4cd3-9944-b2403e98b72b', devotion_dissolves_birth_barriers, theological).
narrative_ontology:cs_axiom('aa5f54b8-254a-4cd3-9944-b2403e98b72b', foundational, surrender_supersedes_social_role_dharma).
narrative_ontology:cs_axiom_status(surrender_supersedes_social_role_dharma, holdable).
narrative_ontology:cs_axiom_grounding('aa5f54b8-254a-4cd3-9944-b2403e98b72b', surrender_supersedes_social_role_dharma, theological).
narrative_ontology:cs_axiom('aa5f54b8-254a-4cd3-9944-b2403e98b72b', secondary, battlefield_frame_subordinate_to_devotion).
narrative_ontology:cs_axiom_status(battlefield_frame_subordinate_to_devotion, holdable).
narrative_ontology:cs_axiom_grounding('aa5f54b8-254a-4cd3-9944-b2403e98b72b', battlefield_frame_subordinate_to_devotion, conventional).
narrative_ontology:cs_reference_frame('aa5f54b8-254a-4cd3-9944-b2403e98b72b', universal_bhakti_direct_access).
narrative_ontology:cs_drift_state('aa5f54b8-254a-4cd3-9944-b2403e98b72b', contemporary_post_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa5f54b8-254a-4cd3-9944-b2403e98b72b', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, twice_born_patron_elites).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, shudra_devotees).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, outcaste_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, women_of_devout_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, cross_caste_bhakti_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, cross_caste_bhakti_communities).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, birth_determines_spiritual_standing).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, priestly_mediation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officiates the rites of birth, marriage, death, and harvest; decides who may study the Veda, who may enter which temple, and who must be served through mediation. Collects dakshina, ritual fees, land grants, and daily deference. Its caste standing, livelihood, and self-understanding are all constituted by the order it administers; stepping outside it would mean surrendering the identity that only the order confers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, brahmin_priestly_class, beneficiary).

% Kings, landholders, and merchant houses endow temples, fund festivals, and pay for the rites that sanctify their rank. They can and do shift patronage among sects and lineages, but the rank the order certifies for them exists only inside it; their standing is purchased here or not at all.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, twice_born_patron_elites, beneficiary,
    powerful, generational, constrained, continental).

% Labor the fields and workshops that fund the temple economy. Barred from Vedic recitation, they receive rites only through a priest, at a fee, in forms the priesthood prescribes. Devotion is open to them as solace and song; caste exit is not open at all, and ritual standing cannot be earned, only inherited.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, shudra_devotees, payer,
    powerless, generational, trapped, continental).

% Stand wholly outside the gate: barred from temple entry, denied the services that mark life's passages, assigned the pollution-bearing labor the purity code requires and the dignity it forbids. The text's own verses — on this reading — name them first among those devotion reaches; the order they live under names them last, or not at all.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, outcaste_communities, payer,
    powerless, generational, trapped, continental).

% Barred from Vedic recitation and from most officiant roles; their licensed religious channel is the household shrine, the fast, and the vernacular song. The universalist verses include them explicitly among those the path of devotion reaches; the gatekeeping order includes them among those who may not touch the text.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_of_devout_households, payer,
    powerless, biographical, constrained, continental).

% Organize devotion across caste lines — singing, pilgrimage, teacher lineages open to any who come. They demonstrate in practice what they read in the text, and pay for it: sanction, exclusion from orthodox rite, occasional persecution. Their warrant comes from the canon the gatekeeping class administers, which is why the establishment absorbs their songs while refusing their conclusion.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, cross_caste_bhakti_communities, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, cross_caste_bhakti_communities, beneficiary).

% Nammalvar, Kabir, Ravidas, Tukaram, Mirabai: low-caste and women poets who sang the text's teaching in the people's languages and lived outside the gate. They had everything to say to the interpretive conversation and no standing in it; the establishment canonized some of their songs and refused all of their conclusions. Their identity was their message; recanting was not something they could do and remain themselves.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, vernacular_saint_poets, excluded,
    moderate, biographical, identity_locked, continental).

% Administers the erosion of the gate from outside the religious economy: constitutional equality, temple-entry statutes, anti-untouchability law. It cannot leave the field unregulated, and it cannot regulate belief, only access — so the gate it dismantles legally persists socially, and its enforcement of access meets the order's enforcement of exclusion.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, constitutional_reform_state, agenda_setter,
    institutional, generational, constrained, national).

% Reads the text's strata, dates its layers, and watches the reading contest from outside every confessional seat. It can see that the order's warrant and the text's teaching pull in opposite directions; it collects no fee and pays none, and its findings are available to every party and owned by none.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, textual_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates ritual and social life across the subcontinent: a hereditary priesthood solves life-cycle rites, festival calendars, textual transmission, and purity management once, centrally, while caste boundaries order the division of ritual labor and marriage exchange.
% TRANSFER_FUNCTION: Moves dakshina (ritual fees), gifts, land grants, labor, and everyday deference from laboring castes, outcaste communities, and patrons to the priestly establishment; moves ritual access and spiritual assurance back only in rationed, mediated form; moves interpretive authority upward to the gatekeeping class.
% ABSENT_VOICES: The excluded devotees — shudra laborers, outcaste communities, women — held no seat in the interpretive conversations that fixed the text's authoritative meaning. The vernacular saint-poets spoke the universalist claim from outside the gate in vernacular song and were marginalized, absorbed, or domesticated; anti-caste reformers were ruled out of order entirely. Their absence is not accidental: admitting their testimony — that devotion needs no gate — dissolves the arrangement under examination.
% DISAPPEARANCE_RATIONALE: If the gatekeeping order vanished overnight, ritual life would reorganize around direct devotional access — as the bhakti movements repeatedly demonstrated at scale — priestly rents and hereditary office would evaporate, and the status hierarchy would lose its religious warrant; marriage networks, temple economies, and village ritual labor would all re-form. The world rearranges; this reading's claim is precisely that the text's own teaching would survive the rearrangement intact.
% FOUNDING_PROBLEM: The order was built to coordinate a large agrarian society's ritual life: who may perform which rite, how the Vedic corpus is transmitted without print or mass literacy, how a division of labor and marriage exchange is stabilized — with rationed spiritual access serving as the enforcement currency that made the whole system self-policing.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the text's own universalist stratum (9.26–9.32 — a leaf, a flower, fruit, or water offered by anyone; 18.66 — abandon all dharmas and take refuge in me) attests that access was never meant to be rationed; the vernacular saint-poets attest across five centuries, from outside the gate, that devotion functioned without mediation; modern philology attests that the text's composite strata postdate the rigid gate it is cited to support. The establishment itself attests only the coordination half, not the rationing — no source outside the benefiting parties attests that spiritual access requires the gate.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 (interval end): the order collects ritual fees, land, labor, and deference from those whose full access it denies, and the deepest charge is psychic — birth-based spiritual worth, taught early and total. Suppression is 0.60 at interval end: enforcement capacity (purity codes, temple bans, sanction against cross-caste worship and study) has eroded substantially from its late-medieval peak under bhakti normalization, colonial-era law, and constitutional equality, but the practices persist where enforcement receded. Theater_ratio 0.42: a substantial share of observance is rank-performance — purity display and precedence — alongside real functions (life-cycle rites, festival coordination, textual transmission). Accessibility_collapse 0.50: alternatives did not fully collapse — the devotional exit (bhakti across caste, renunciation, heterodox paths) remained reachable, and that standing exit is precisely the lever this reading pulls. Resistance 0.70: centuries of bhakti movements — the coalition form that powerless victims actually took, and the reason the gate never fully closed — plus anti-caste assertion and reform legislation. All three tracked series run on ONE shared grid (t = 0, 20, 40, 60, 80, 100, 120). Suppression_requirement is tracked because enforcement-capacity change is a central dynamic of this story: the gate hardens through medieval consolidation (0.55 to 0.76), then erodes under bhakti, reform, and constitutional law (to 0.60) while extraction decays more slowly (peak 0.80 to 0.74) — the divergence between enforcement decay and rent persistence is the story's open question (see the gate_persistence_without_warrant omega). The trajectory is rise-and-erode, not cyclical. Suppression is authored as a raw structural property and is never scaled; extractiveness is the engine's to scale by directionality and spatial scope — the order's continental scope amplifies verification difficulty for the excluded.
 *
 * PERSPECTIVAL GAP:
 *   From the priestly seat the order is sacred duty: the gate protects the text's transmission and the rite's efficacy, and the universalist reading is heresy or naivety. From the excluded seats the same structure is rationed salvation: the fee, the mediation, the ban. The patron seat experiences the order as a service it purchases — legitimation — and prices accordingly. The bhakti communities hold a genuinely dual position visible only through their declared roles: they pay sanction costs as payers while drawing their warrant from the canon the gatekeeping class administers. The engine computes per-seat classifications from these structural data; the divergence between the priestly seat's computed experience and the outcaste seat's is the measurement, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end: the priestly class collects the fees, land, and deference and administers the rules it collects under — its identity lock binds it TO the arrangement as administrator, not against it as victim, so the lock does not push it toward the target end. Patron elites purchase legitimation and retain some patronage arbitrage across sects, damping their d further. Victims sit near the target end, ordered by exit: outcaste communities are trapped with no standing at all (nearest full target), shudra devotees trapped with rationed access, women constrained to a licensed devotional channel. Cross-caste bhakti communities derive a mid-to-high d from their payer role, damped by their secondary benefit (canon warrant). No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct d for every seat, and the one candidate override (the priestly class) resolves correctly through its role and exit declarations alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification holds two mislabels apart. Calling the order a pure snare erases the coordination that is real and load-bearing — ritual life, textual transmission, and the festival calendar all function and would need reorganizing overnight (hence disappearance_verdict: world_rearranges). Calling it a rope erases the asymmetric capture this reading exists to name — the gains land demonstrably in one seat (gain_flow: brahmin_priestly_class) and the gate is defended by that seat at any cost (fixing_cost: prohibitive). The R5 genealogy splits cleanly: the founding problem's coordination half is live (rites, transmission, social order persist), its rationing half is dead-as-necessity and alive-as-rent-defense — hence founding_problem_status: contested, corroborated from outside the beneficiary set by the text's own universalist stratum, the vernacular saint-poets, and modern philology. Because the beneficiary seat demonstrably captures, the arrangement is not drifting piton-ward despite falling enforcement; the open question is whether it decays with its warrant or persists inertially after the warrant goes — the gate_persistence_without_warrant omega tracks exactly that divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is the universalist_devotional_reading of the gita_kurukshetra_discourse kernel. The sibling readings — orthodox_literal_reading (the text mandates caste duty and legitimates dharmic violence) and gandhian_allegorical_reading (Kurukshetra is inner struggle) — would author different victim sets and different ε over the same standing arrangement. Where exactly is the disagreement located, and can any data settle it?',
    'The disagreement is located in the dharma verses (2.31–2.47, 18.41–18.48) versus the surrender verses (9.26–9.32, 18.66): whether the latter override the former or merely console alongside them. Philology can date strata; it cannot by itself settle which stratum governs — adoption is settled by which reading communities institutionalize, not by manuscripts alone.',
    'Under the orthodox sibling the same order authors as low-ε dharma with the gate as owed duty and no victims of the gate; under the gandhian sibling the violence question relocates inward and the caste question attenuates. This story''s high ε, its victim set, and its tangled_rope claim are reading-indexed to the universalist seat over the shared referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; ε is reading-indexed over the shared standing arrangement.').

omega_variable(
    textual_stratification,
    'Is the universalist devotional stratum (the bhakti chapters, 9.26–9.32, 18.66) integral to the text''s core message, or a later accretion layered onto an older caste-duty core?',
    'Philological and manuscript-stratification analysis of the text''s layers and their dates relative to the codification of the four-varna gate.',
    'If accretion, the orthodox sibling''s warrant strengthens and this reading''s claim weakens to ''the text contains a dissolving minority report''; if integral, the standing arrangement stands against the text''s own center and the gate''s textual warrant collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stratification, empirical, 'Whether the universalist stratum is core or accretion.').

omega_variable(
    surrender_autonomy_reflexivity,
    'Does the endorsed alternative — total surrender (prapatti) as the universal path — carry its own extraction, trading autonomy and discernment for assurance, such that a universal-devotion order would eventually develop its own gates?',
    'Compare realized devotional communities organized on open-access devotion for rent formation: do new intermediaries, fees, or orthodoxy enforcement emerge around surrender itself?',
    'If surrender demands extract, the delta''s ''universal devotee class'' beneficiary claim weakens and any successor arrangement trends toward hybrid coordination rather than pure; if not, the reading''s endorsed alternative is genuinely low-ε and the gate''s extraction is attributable to the birth-principle specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surrender_autonomy_reflexivity, conceptual, 'Reflexive check: whether the reading''s own endorsed alternative carries extraction.').

omega_variable(
    gate_persistence_without_warrant,
    'If the gate''s textual warrant dissolves (via stratification findings or reception), does the arrangement persist on pure inertia — drifting from hybrid coordination toward inertial performance or hardened capture — or does it dissolve with its warrant?',
    'Track enforcement capacity and rent flows against warrant erosion: the authored suppression_requirement series already shows enforcement decaying faster than extraction; continued divergence with rents intact indicates inertial persistence.',
    'Inertial persistence would date a drift toward piton (theater rising, function atrophying, no one hurt enough to fix it); collapse-with-warrant would date a resolution. The current series shows the opening of that divergence, not its outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gate_persistence_without_warrant, empirical, 'Whether the gate outlives its textual warrant.').

omega_variable(
    internalized_birth_worth,
    'Is the arrangement''s suppression structural (temple bans, ritual fees, purity enforcement) or internalized (devotees'' belief that birth limits their standing), and in what proportion?',
    'Post-reform uptake trajectory: where legal access was granted (temple-entry statutes, constitutional equality), did excluded communities take it up immediately, or did uptake lag for generations? Lag with access open indicates internalized suppression persisting after the structural barrier fell.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts legal reform — the arrangement would survive its own deregulation, and the erosion tail in the suppression series would flatten rather than reach zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_birth_worth, empirical, 'Structural versus internalized suppression mechanism in the caste gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.46).
narrative_ontology:measurement(gita_tr_t120, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.77).
narrative_ontology:measurement(gita_be_t120, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 120, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(gita_su_t120, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 120, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Gita's teaching' decomposes, per the ε-invariance principle, into three structurally distinct constraint stories from one kernel: the orthodox literal reading (caste duty mandated, dharmic violence legitimated), the gandhian allegorical reading (battlefield as inner struggle), and this universalist devotional reading. All three share one ε referent — the standing caste-gated devotional order — and author different ε over it; forcing one story to carry all three would make ε observer-dependent. This story links both siblings via network edges: the family's upstream claim (the text's authority) is cited by each sibling as warrant for incompatible arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
