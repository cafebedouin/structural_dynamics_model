% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition — Functional Coexistence Reading (Kami This-World / Buddhas Afterlife)
 *   domain: religious studies/japanese history/commitment systems
 *
 * SUMMARY:
 *   For roughly eleven centuries (741-1868) Japanese religious life ran on a
 *   jurisdictional settlement: kami cults administered this-worldly concerns
 *   — rain, harvests, epidemics, festival life — while Buddhist institutions
 *   administered the afterlife — funerals, memorials, graves, karmic welfare.
 *   The settlement was sanctioned by the ritsuryo court, deepened by medieval
 *   shrine-temple complexes, and hardened in the Edo period into a compulsory
 *   registration regime (the terauke/danka system) binding every household to
 *   a parish temple. This story instantiates the domain_partition_reading of
 *   the shinbutsu_ontological_substrate kernel: it authors the arrangement as
 *   pragmatic functional coexistence — coordination between two religious
 *   systems with distinct domains, not ontological fusion. Claim and metrics
 *   are independent: the reading claims rope (functional coordination, low
 *   entanglement, easy separation); the authored metrics describe the
 *   arrangement's end-state operation, in which an Edo-era compulsory-fee
 *   layer rode on the functional core. The sibling readings are separate
 *   constraints over the same referent with their own epsilon values (see
 *   kernel_context and network); this file does not adjudicate between them.
 *
 * KEY AGENTS:
 *   - tokugawa_bakufu: end-state agenda_setter (institutional/arbitrage) — built and enforced the terauke registration system, collected governance benefits (population control, Christian suppression)
 *   - imperial_court: founding agenda_setter (institutional/arbitrage) — chartered the shrine-temple settlement, regulated ordination, collected order and legitimacy
 *   - buddhist_danka_temples: primary beneficiary (organized/constrained) — hold the funerary monopoly and the parish fee stream; the seat the extraction accrues to
 *   - kami_priesthoods: secondary beneficiary (moderate/identity_locked) — hereditary lineages administering the this-worldly domain on stipends and offerings
 *   - lay_patron_communities: dual seat (moderate/constrained) — receive the divided services and fund both institutions; compulsory under the parish system
 *   - danka_households: primary target (powerless/trapped) — compulsory registration, hereditary parish bond, funerary fees under Christian-suspicion penalty
 *   - shugenja_practitioners: boundary-crossing target (moderate/identity_locked) — blended practice with no place in the jurisdictional settlement
 *   - kokugaku_scholars: excluded critic (moderate/constrained) — kami-exclusive program outside the settlement; supplied the 1868 separation's intellectual case
 *   - religious_studies_historians: analytical observer (analytical/analytical) — reconstructs the structure from charters, land records, and registration rolls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.52).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Domain Partition — Functional Coexistence Reading (Kami This-World / Buddhas Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious studies/japanese history/commitment systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '66a7c69a-269d-4b11-907f-768fd98ebe26').
narrative_ontology:cs_kernel_codification('66a7c69a-269d-4b11-907f-768fd98ebe26', distributed).
narrative_ontology:cs_authority_grounding('66a7c69a-269d-4b11-907f-768fd98ebe26', practice).
narrative_ontology:cs_interpretation_layer_present('66a7c69a-269d-4b11-907f-768fd98ebe26').
narrative_ontology:cs_reading_relation('66a7c69a-269d-4b11-907f-768fd98ebe26', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('66a7c69a-269d-4b11-907f-768fd98ebe26', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('66a7c69a-269d-4b11-907f-768fd98ebe26', foundational, coexistence_is_functional_not_ontological).
narrative_ontology:cs_axiom_status(coexistence_is_functional_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('66a7c69a-269d-4b11-907f-768fd98ebe26', coexistence_is_functional_not_ontological, empirically_contingent).
narrative_ontology:cs_axiom('66a7c69a-269d-4b11-907f-768fd98ebe26', secondary, kami_this_world_buddhas_afterlife_division).
narrative_ontology:cs_axiom_status(kami_this_world_buddhas_afterlife_division, holdable).
narrative_ontology:cs_axiom_grounding('66a7c69a-269d-4b11-907f-768fd98ebe26', kami_this_world_buddhas_afterlife_division, conventional).
narrative_ontology:cs_reference_frame('66a7c69a-269d-4b11-907f-768fd98ebe26', functional_jurisdiction_partition).
narrative_ontology:cs_drift_state('66a7c69a-269d-4b11-907f-768fd98ebe26', meiji_separation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('66a7c69a-269d-4b11-907f-768fd98ebe26', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, kami_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_danka_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_patron_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, danka_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, shugenja_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_bakufu).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, lay_patron_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, kami_buddha_compatibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sanctioned the shrine-temple settlement from the Nara period onward: chartered shrine-temple complexes, regulated Buddhist ordination through the Office of Priestly Affairs, and assigned kami cults and Buddhist institutions complementary roles in the state cult. Collected legitimacy, ritual order, and a handle on tax-exempt land accumulation rather than ritual fees. Could restructure the settlement by charter and did so repeatedly across the classical and medieval periods.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% From the 1630s built and enforced the temple certification system: every household had to register with a Buddhist parish temple and obtain certificates for marriage, travel, and house-building. Used the registration network to suppress Christianity and track the population. Collected governance benefits — population control, Christian suppression, local surveillance — while leaving the funerary fee stream to the temples. Held enforcement supremacy over the arrangement until 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_bakufu, beneficiary).

% Hereditary shrine lineages administering the this-worldly domain: agricultural rites, purification, protection from misfortune, festival life. Supported by court and bakufu stipends and local offerings. The priestly office is tied to lineage — leaving it means abandoning the family's sacred charge — but the domain itself was secure: the settlement assigned the living-world rites to shrines and left them uncontested there. Major shrines such as Ise and Izumo carried institutional weight; village priesthoods lived on local offerings.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, kami_priesthoods, beneficiary,
    moderate, generational, identity_locked, national).

% Buddhist parish temples holding the afterlife domain: funerals, memorial services, graves. In the medieval period many expanded landholdings through shrine-temple complexes built on shrine lands. Under the Edo parish system they collected mandatory fees from registered households and held certification power over family legal acts; the parish roll was their revenue base and legal standing, and sect hierarchies disciplined them from above. They are the seat the funerary fee stream lands on.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_danka_temples, beneficiary,
    organized, generational, constrained, national).

% Village and town communities using the divided system: shrines for rain, harvests, epidemics, and festivals; temples for funerals, graves, and memorials. They funded both institutions through offerings, labor, and later mandatory parish fees, and received working services from each. Patronage was voluntary for most of the interval and compulsory under the parish system; there was no third institution for rites that crossed the domain line.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_patron_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, lay_patron_communities, payer).

% Households registered to a parish temple under the certification system. They paid funeral and memorial fees, maintained family graves on temple grounds, and needed the temple's certificate for marriages, travel permits, and building. The parish bond passed to heirs; a household could not deregister without exposing itself to Christian-suspicion prosecution. Fee resentment accumulated for generations and fed the anti-Buddhist backlash of 1868-71.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, danka_households, payer,
    powerless, generational, trapped, national).

% Mountain ascetic orders whose practice fuses kami worship and Buddhist esoterics: mountain kami revered as buddha manifestations, sutras recited at shrines, ascetic rites that cross the domain line the settlement maintains. Recognized as licensed corporations under both institutional wings but represented in neither; their blended practice was anomalous under the settlement's jurisdictional logic and was banned outright by the 1868 separation edicts, which dissolved their orders.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shugenja_practitioners, payer,
    moderate, generational, identity_locked, regional).

% National-learning scholars arguing that kami worship is self-sufficient and that Buddhist framing is foreign contamination to be stripped away. The settlement presupposes both systems' legitimacy, so their program had no place in it and no seat in its governance. Their critique supplied the intellectual case the Meiji state used for the 1868 separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, kokugaku_scholars, excluded,
    moderate, generational, constrained, national).

% Reconstruct the arrangement from charters, land records, registration rolls, and edict texts across its full interval. Neither collect nor pay; work outside the tradition's own categories and can see the jurisdictional core, the doctrinal layers, and the parish-era enforcement as distinct strata of one structure.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_danka_temples).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual jurisdiction between two religious systems so that they complement rather than compete: kami cults supply this-worldly rites (rain, harvests, purification, protection from misfortune, festival life) while Buddhist institutions supply soteriological services (funerals, memorials, graves, karmic welfare). A lay community can address both need-domains without choosing between the systems or triggering jurisdictional conflict.
% TRANSFER_FUNCTION: Moves offerings, corvee labor, land income, and — in the Edo period — compulsory funerary and memorial fees from lay patrons and registered households to shrine and temple institutions; moves certification power over family legal acts from households to parish temples under bakufu enforcement; moves legitimacy and ritual order to the court and bakufu.
% ABSENT_VOICES: Kami-exclusive voices — kokugaku scholars and Ise-tradition purists — would object that the settlement legitimizes any Buddhist presence in kami worship at all and would dissolve the dual system entirely; they had no seat in the jurisdictional arrangements. Boundary-crossing practitioners (shugenja) had no representation in the settlements that defined their practice as anomalous. Danka households had no collective voice in setting the fee levels they paid.
% DISAPPEARANCE_RATIONALE: Removal forces every household to re-source funerary and memorial rites, every shrine to redefine its cult outside the settlement, every parish temple to lose its revenue base, and the state to rebuild its registration infrastructure. This is not hypothetical: the 1868 separation edicts produced exactly this rearrangement, violently — the haibutsu kishaku wave destroyed or consolidated thousands of temples, laicized thousands of priests, and forced the new state to moderate its own policy within a few years to restore order.
% FOUNDING_PROBLEM: The ritsuryo state needed to integrate indigenous kami cults — the legitimacy and agricultural rites of the realm — with imported Buddhism — state protection, soteriology, literacy — without letting the two systems compete for the same ritual functions, patronage, or court favor. The jurisdictional division assigned each system a domain so that coexistence would be structural rather than contested.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the documentary record outside any beneficiary's self-account: ritsuryo court charters and edicts establishing the shrine-temple settlement, and the Office of Priestly Affairs' ordination controls, show the state explicitly managing competition between the two systems. Its death is attested from outside the beneficiary set: the Meiji separation edicts declared the coexistence arrangement itself illegitimate; kokugaku scholarship had argued for decades that the settlement was contamination rather than integration; and modern historiography documents that by the Edo period the arrangement's operative function was compulsory registration rather than management of rival systems. The beneficiary parties attested the opposite — that the settlement remained necessary — which is the cover-story signature the corroboration rule exists to catch.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.52 because the arrangement's final centuries ran a compulsory funerary economy: parish temples collected mandatory fees from households that could not lawfully deregister, on top of the medieval layer of land-based institutional income. Suppression ends at 0.58 because the parish bond was state-enforced — the terauke certificate was required for marriage, travel, and building, and non-registration exposed a household to Christian-suspicion prosecution; suppression is authored as a raw structural property and is not scaled by power or scope (the engine scales only extractiveness, by directionality and scope). Theater ends at 0.33: the ritual services remained real (funerals, memorials, festivals were genuinely performed and used), but a growing share of institutional activity was registration formalism — maintaining rolls and certificates rather than performing rites. Accessibility collapse 0.62: a household inside the system had no usable alternative — no shrine-only funerals, no lawful non-affiliation — though sect choice and discursive kami-exclusivism survived. Resistance 0.55: kokugaku critique, village disputes over temple levies, and finally the 1868-71 anti-Buddhist backlash that destroyed thousands of temples. All three series share one time grid (741, 900, 1100, 1300, 1500, 1700, 1800, 1868); suppression_requirement is authored because the narrative specifically tracks enforcement-capacity change (court charter-and-ordination control giving way to bakufu compulsory certification). The dynamics are a monotonic hardening, not a cycle. The claimed type (rope) is the reading's structural claim about the arrangement's core; the end-state metrics describe what actually operated — the engine computes per-seat types from the structural data, and where the computed type diverges from the rope claim, that divergence locates the extraction in the Edo accretion rather than the jurisdictional core, which is precisely the measurement this reading exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the danka household's position the arrangement is compulsory extraction: a hereditary fee bond enforced by the bakufu with criminal penalties, exit effectively impossible. From the parish temple's position the same bond is livelihood and parish structure — the danka roll is the temple's revenue base and legal standing. From the kami priesthood's position the arrangement is secure jurisdiction: the this-worldly domain is theirs by settlement, and the temple monopoly on funerals leaves the living-world rites uncontested. From the bakufu's position the arrangement is governance infrastructure — a registration net covering every household. Two agenda-setters across the interval used different instruments: the ritsuryo court governed by charter and ordination control; the bakufu governed by compulsory certification — inter-institutionally, the same settlement was experienced as charter-granting order by one regime and as surveillance infrastructure by the next. Same-level: the kami priesthoods and the Buddhist temples held similar nominal religious standing but structurally different positions — the priesthoods' domain was secure without coercion, while the temples' domain rested on a state-enforced monopoly that made them the fee stream's recipients and, ultimately, the backlash's target. Coalition dynamics matter for the powerless seats: danka households were individually trapped, but their accumulated resentment became politically decisive when the Meiji state allied it with kokugaku ideology in 1868 — the coalition that destroyed the arrangement was built from its own victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. buddhist_danka_temples are the gain_flow seat — the funerary fees and certification power accrue to them — so their d sits near the beneficiary end. kami_priesthoods benefit from secure jurisdiction and stipends; their identity_locked exit reflects hereditary sacred office, not cost-bearing, so d stays low. danka_households are the primary target: trapped exit (deregistration exposed a household to prosecution) puts them near the full-target end. shugenja_practitioners bear the boundary logic itself — their blended practice is what the jurisdictional settlement excludes — and their identity_locked exit (the blend is constitutive of the practice) keeps them near the target end. lay_patron_communities hold a genuinely dual position — they receive the divided services and fund both institutions — so their derived value from the beneficiary listing alone would sit too low; the secondary payer role and the parish-era compulsion place them near symmetric, and the engine reads this from the declared roles and constrained exit. imperial_court and tokugawa_bakufu appear in neither the beneficiary nor the victim arrays (they collect order and governance rather than ritual fees), so the derivation would fall back to a canonical default that misses their mild-beneficiary position; a single institutional-atom override to 0.35 covers both, since both sit at that atom with the same structural relationship. kokugaku_scholars bear the arrangement's suppression of their program; as an excluded seat with no array data their position is documented here rather than overridden, since the moderate atom is shared with structurally different seats (priesthoods, shugenja, lay patrons) and an atom-level override there would misstate all of them. The national spatial scope of the fee system makes verification of fee burdens harder and amplifies effective extraction for the trapped seats; the engine owns that scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping two mislabelings apart. Reading the arrangement as pure coordination dissolves the danka households' situation — a compulsory hereditary fee bond — into a harmonious division of labor. Reading it as pure extraction erases the millennium in which the jurisdictional division genuinely solved the integration problem and required almost no coercion. The founding problem — ordering kami cults and Buddhist institutions so they would not compete for the same rites — is dead: by the Edo period the settlement's operative function was registration infrastructure, not management of rival systems, and the doctrinal frame had long since passed to the fusion reading's territory. The R5 mismatch (dead founding problem, world-rearranging removal) flags the end state as captured: the arrangement persisted under enforcement after its founding function had transformed, and it ended by force rather than atrophy. What prevents the mislabel is the layered seat structure: the coordination function and the extraction function are carried by different strata of the same arrangement, and the temporal series shows the extraction layer arriving a millennium after the coordination core. Identity-lock dynamics are constitutive at two seats: the kami priesthoods' lock is institutional-lineage (the office is the family's sacred charge, inalienable across generations), and the shugenja's lock is practice-constitutive (their discipline IS the kami-buddha blend, so exiting the arrangement means ceasing to exist as a school) — if either lock broke, those seats' classifications would shift sharply, and in fact the 1868 edicts broke the shugenja lock by force and dissolved the school.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the domain_partition_reading of the shinbutsu_ontological_substrate kernel; would instantiating the syncretic_fusion_reading or the incoherent_bundle_reading change the structural classification of the same historical arrangement?',
    'Author the sibling readings as separate constraint stories over the same referent and compare per-seat classifications: the fusion reading should author deeper institutional entanglement and a different victim set; the bundle reading should author no coordination function at all.',
    'If the fusion reading computes a more extractive type for the same seats, the extraction is located in the ontological-fusion layer rather than the jurisdictional division; if the bundle reading computes no coordination, the partition reading''s rope claim loses its coordination warrant entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings over one kernel may classify the same arrangement differently.').

omega_variable(
    separation_cost_dispute,
    'Was separation of kami and buddha worship structurally cheap, as this reading''s low-entanglement and easy-separation claims imply, or did accumulated institutional entanglement make separation destructive?',
    'The 1868-71 record: the shinbutsu bunri edicts triggered haibutsu kishaku — on the order of four and a half thousand temples destroyed or consolidated, thousands of priests forcibly laicized, Buddhist artifacts burned; if separation required this scale of destruction, the low-entanglement premise fails for the arrangement as it actually stood.',
    'If separation was destructive, the reading''s rope claim loses its easy-separation warrant and the end state computes closer to an enforced hybrid; the reading''s fallback — that the destruction cost came from the parish-era extraction layer rather than the partition core — becomes load-bearing and independently testable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separation_cost_dispute, empirical, 'Whether the historical separation outcome falsifies the reading''s easy-separation premise.').

omega_variable(
    danka_accretion_vs_partition_core,
    'Is the end-state extraction — compulsory funerary registration and fees — an accretion on the jurisdictional partition, or constitutive of the arrangement from its founding?',
    'Compare extraction levels across the interval: the partition operated for roughly nine centuries on voluntary patronage, land income, and court stipend before the terauke system (1630s onward) made registration compulsory; if the pre-Edo arrangement ran at low extraction, the accretion reading holds.',
    'If accretion, the partition core remains coordination-classifiable with an extraction rider that arrived late; if constitutive, the whole arrangement computes as a hybrid from the founding and the reading''s functional claim covers only a fraction of its history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_accretion_vs_partition_core, empirical, 'Whether the Edo extraction layer is separable from the partition''s coordination core.').

omega_variable(
    honji_suijaku_level_question,
    'Does the doctrinal dominance of honji suijaku — kami as manifestations of buddhas — falsify the claim that coexistence was functional rather than ontological, or did doctrinal fusion and functional division operate at different levels without contradiction?',
    'Test whether lay practice tracked the doctrinal ontology: if patronage patterns followed the domain division (shrines for this-worldly needs, temples for funerals) regardless of doctrinal profession, the two levels were structurally independent; if practice internalized the fusion ontology, the partition describes a surface only.',
    'If the levels were independent, the partition reading survives as a practice-level description and the fusion reading captures only the doctrinal layer; if practice internalized fusion, this reading''s epsilon is indexed to a surface and the fusion reading holds the real structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_level_question, empirical, 'Whether doctrine and practice were independent levels or the fusion ontology governed practice.').

omega_variable(
    danka_bond_internalization,
    'Was household compliance with the parish bond purely structural compulsion, or partly internalized as ancestral obligation that would persist after the compulsion was removed?',
    'Post-1871 behavior: after the Meiji state abolished the compulsory registration system, most households continued temple affiliation and grave maintenance voluntarily; the persistence rate after lawful exit became available measures the internalized share.',
    'If substantially internalized, the arrangement''s suppressive force outlived its enforcement machinery and the structural suppression measure understates the constraint''s hold; if compliance collapsed once compulsion lifted, the bond was purely structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_bond_internalization, empirical, 'Structural versus internalized component of danka compliance — suppression mechanism ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 741, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t741, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 741, 0.08).
narrative_ontology:measurement_basis(shin_tr_t741, observed).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement_basis(shin_tr_t900, observed).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement_basis(shin_tr_t1100, observed).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1300, 0.15).
narrative_ontology:measurement_basis(shin_tr_t1300, observed).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement_basis(shin_tr_t1500, observed).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1700, 0.26).
narrative_ontology:measurement_basis(shin_tr_t1700, observed).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement_basis(shin_tr_t1800, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.33).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t741, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 741, 0.22).
narrative_ontology:measurement_basis(shin_be_t741, observed).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 900, 0.28).
narrative_ontology:measurement_basis(shin_be_t900, observed).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1100, 0.34).
narrative_ontology:measurement_basis(shin_be_t1100, observed).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1300, 0.38).
narrative_ontology:measurement_basis(shin_be_t1300, observed).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement_basis(shin_be_t1500, observed).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1700, 0.5).
narrative_ontology:measurement_basis(shin_be_t1700, observed).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement_basis(shin_be_t1800, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.52).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t741, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 741, 0.12).
narrative_ontology:measurement_basis(shin_su_t741, observed).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 900, 0.15).
narrative_ontology:measurement_basis(shin_su_t900, observed).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1100, 0.18).
narrative_ontology:measurement_basis(shin_su_t1100, observed).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1300, 0.22).
narrative_ontology:measurement_basis(shin_su_t1300, observed).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.28).
narrative_ontology:measurement_basis(shin_su_t1500, observed).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement_basis(shin_su_t1700, observed).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1800, 0.54).
narrative_ontology:measurement_basis(shin_su_t1800, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1868, 0.58).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (shinbutsu_ontological_substrate), three readings, one file per reading per the epsilon-invariance principle. This file instantiates the domain_partition_reading — epsilon authored for the standing arrangement as a functional jurisdictional division with a separable core. The syncretic_fusion_reading instantiates the same arrangement as ontological unity (honji suijaku as metaphysical truth) and should author deeper entanglement and a different victim structure; the incoherent_bundle_reading denies the kernel's coherence entirely and should author no coordination function. The epsilon values differ because the readings differ, not because the referent does; the family exists so that divergence is measurable rather than averaged away.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__domain_partition_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
