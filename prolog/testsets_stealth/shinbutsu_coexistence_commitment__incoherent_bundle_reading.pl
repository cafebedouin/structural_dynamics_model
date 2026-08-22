% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [RESOLVED — COLLAPSED UNDER MEIJI BUNRI (1868)]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-Shugo as Incoherent Institutional Bundle (Pre-Meiji Composite Arrangement)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   From roughly the tenth century to 1868, kami shrines and Buddhist temples
 *   across Japan formed composite institutions: shrines with attached
 *   temples, kami read as local traces of buddhas, clergy, revenues, and
 *   rites flowing across the boundary. This story instantiates the
 *   incoherent_bundle_reading of that arrangement: the claim is that
 *   shinbutsu-shugo was never a coherent theological kernel but a bundle of
 *   institutional accommodations sustained by deliberately avoiding the
 *   categorical question — what, finally, is a kami in relation to a buddha?
 *   — and by the concentrated power of the monastic establishments that
 *   monopolized the answer's absence. On this reading the Meiji separation
 *   edicts did not destroy a working synthesis; they asked the forbidden
 *   question by fiat, and the arrangement, having no doctrinal floor,
 *   collapsed almost immediately. The epsilon referent is the standing
 *   pre-Meiji composite arrangement, assessed by this reading's own lights —
 *   not the separated Shinto/Buddhist order built afterward, and not the
 *   fused order the sibling readings describe. Claim and metrics are authored
 *   independently: the claimed type states what this reading takes to be
 *   structurally true; the metrics describe the arrangement's operation as
 *   the historical record shows it. KEY AGENTS (by structural relationship):
 *   buddhist_monastic_establishments — agenda-setting beneficiary
 *   (institutional/arbitrage); warrior_state_administrators — secondary
 *   beneficiary with enforcement interest (institutional/arbitrage);
 *   shrine_priesthoods_in_temple_hierarchies — embedded beneficiary
 *   (moderate/constrained); kami_cult_village_communities — primary payer
 *   (powerless/constrained); independent_kami_priest_lineages — payer locked
 *   by lineage identity (moderate/identity_locked); peasant_danka_households
 *   — payer under compulsory registration (powerless/trapped);
 *   kokugaku_yoshida_reformers — excluded objectors;
 *   meiji_separation_edict_authors — excluded counter-coalition;
 *   religious_studies_historians — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-Shugo as Incoherent Institutional Bundle (Pre-Meiji Composite Arrangement)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '062574b8-1026-40b8-9967-f78d9339e4de').
narrative_ontology:cs_kernel_codification('062574b8-1026-40b8-9967-f78d9339e4de', distributed).
narrative_ontology:cs_authority_grounding('062574b8-1026-40b8-9967-f78d9339e4de', extraction).
narrative_ontology:cs_interpretation_layer_present('062574b8-1026-40b8-9967-f78d9339e4de').
narrative_ontology:cs_reading_relation('062574b8-1026-40b8-9967-f78d9339e4de', shinbutsu_coexistence_commitment__syncretic_fusion_reading, influences).
narrative_ontology:cs_reading_relation('062574b8-1026-40b8-9967-f78d9339e4de', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('062574b8-1026-40b8-9967-f78d9339e4de', foundational, no_operative_ontology_existed).
narrative_ontology:cs_axiom_status(no_operative_ontology_existed, holdable).
narrative_ontology:cs_axiom_grounding('062574b8-1026-40b8-9967-f78d9339e4de', no_operative_ontology_existed, empirically_contingent).
narrative_ontology:cs_axiom('062574b8-1026-40b8-9967-f78d9339e4de', foundational, ambiguity_was_load_bearing_not_accidental).
narrative_ontology:cs_axiom_status(ambiguity_was_load_bearing_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('062574b8-1026-40b8-9967-f78d9339e4de', ambiguity_was_load_bearing_not_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('062574b8-1026-40b8-9967-f78d9339e4de', ambiguous_composite_modus_vivendi).
narrative_ontology:cs_drift_state('062574b8-1026-40b8-9967-f78d9339e4de', meiji_bunri_collapse, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('062574b8-1026-40b8-9967-f78d9339e4de', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, warrior_state_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_priesthoods_in_temple_hierarchies).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kami_cult_village_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, independent_kami_priest_lineages).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, peasant_danka_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Networks of Tendai and Shingon temples that administered shrines through attached chapel-temples, supplied the doctrinal glosses under which kami were read as manifestations of buddhas, appointed and disciplined shrine clergy, and collected dues, land rent, and ritual fees from both sides of every composite site. Their authority rested on being the only institution that explained what kami and buddhas were to each other. Exit for them meant repositioning within the same landscape — shifting estates, affiliations, or glosses — never leaving it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).

% Bakufu and domain authorities from the Kamakura period onward. They taxed and periodically fought the great temple-shrine complexes but relied on the composite arrangement to keep religious jurisdiction legible and to avoid ruling on doctrinal questions. From the seventeenth century they required every household to register with a temple, turning the arrangement into a census and anti-Christian surveillance instrument. Their interest was administrative peace; they arbitrated disputes as a last resort without ever defining the relationship between kami and buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, warrior_state_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, warrior_state_administrators, agenda_setter).

% Hereditary shrine clerics incorporated under parent temples. Association with the great Buddhist houses brought legal protection, pilgrimage traffic, and building patronage; it also placed their rites, succession, and finances under monastic supervision. Leaving the hierarchy meant losing the credentials and protection that made a rural shrine viable.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_priesthoods_in_temple_hierarchies, beneficiary,
    moderate, generational, constrained, regional).

% Village congregations whose local deities were progressively enrolled under nearby temples as manifestation traces. They funded both sides of every composite site — offerings to the kami, dues and funerary fees to the temple — and had no standing in the doctrinal decisions that re-described their gods. Collective refusal surfaced episodically as shrine riots and lawsuits, usually settled by reaffirming the composite with minor concessions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kami_cult_village_communities, payer,
    powerless, generational, constrained, regional).

% Shrine families outside the temple orbit, many attached to the Yoshida licensing system, whose rank, marriage alliances, and ritual repertoire were constituted by hereditary office at a specific sanctuary. Absorption of their shrines into composite sites stripped them of independent standing; abandoning the lineage's office would have dissolved the family's identity and livelihood together, so most accommodated, licensed, and waited.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, independent_kami_priest_lineages, payer,
    moderate, generational, identity_locked, local).

% Under the Edo registration system every household belonged to a temple, certified annually through the temple seal. Membership carried obligatory funerary and memorial services priced by the temple, levies for buildings and festivals, and no lawful way to affiliate elsewhere. Registration was the legal proof of not being Christian; declining it endangered the household.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, peasant_danka_households, payer,
    powerless, biographical, trapped, regional).

% Scholars and shrine reformers — the Yoshida house from the fifteenth century, Kokugaku thinkers from the seventeenth — who argued that kami worship was older than and independent of Buddhism and that the composite arrangement had corrupted it. They published, licensed, and petitioned from outside the monastic establishment; several works were banned, and none held a seat in the administration of any major composite site until the very end of the interval.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_yoshida_reformers, excluded,
    moderate, generational, constrained, national).

% The Restoration leadership that issued the 1868 edicts ordering shrines and temples demerged, expelling Buddhist clergy from shrines and redirecting composite-site revenues to shrine purification. They stood wholly outside the arrangement they dismantled and treated its ambiguities as evidence of corruption rather than of doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_separation_edict_authors, excluded,
    institutional, generational, arbitrage, national).

% Modern scholars of Japanese religion who reconstruct the arrangement from dispute records, registers, and doctrinal texts, and who supply the competing readings — fusion, partition, bundle — among which this story takes one position.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operated a working settlement between two totalizing religious systems sharing one archipelago: it allocated ritual space and jurisdiction (temples administering shrines through attached chapels), prevented zero-sum competition over sites and revenues, and gave the state a single negotiable interface for religious affairs — all without requiring anyone to settle what kami and buddhas ultimately were.
% TRANSFER_FUNCTION: Moved revenue, labor, and ritual obligation from village communities and registered households to temple-shrine complexes and thence to monastic centers; moved legitimacy from kami cults (re-described as subordinate traces) to Buddhist institutions; moved administrative information from households to the warrior state through compulsory temple registration.
% ABSENT_VOICES: Kami-only revivalists — the Yoshida house and the Kokugaku scholars — objected from outside the temple hierarchy and were marginalized or censored; village congregations had no seat in any doctrinal decision that re-described their deities; and the kami themselves, by the tradition's own logic, had no advocate apart from the institutions doing the glossing.
% DISAPPEARANCE_RATIONALE: It effectively did vanish: between 1868 and 1871 the separation edicts triggered the anti-Buddhist destruction — thousands of temples demolished or forcibly merged, tens of thousands of clergy laicized, temple lands confiscated, compound icons burned or thrown into rivers — and a state-centered shrine cult was built on the cleared ground. Every seat rearranged: monastic networks lost their estate base, shrine lineages were refounded as state priesthood, villages acquired separate shrine and funeral institutions, and the state acquired a national cult.
% FOUNDING_PROBLEM: How an imported universal soteriology and an entrenched cult of local deities could occupy the same islands without annihilating each other — answered first by casting kami as protectors of the Buddhist law, later by casting them as manifestation-traces of buddhas.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Kokugaku and Yoshida writers (Motoori Norinaga, Kamo no Mabuchi) attested from the seventeenth and eighteenth centuries that the composite arrangement served monastic power rather than any lived unity of kami and buddhas; modern historiography (Kuroda Toshio's reconstruction of the medieval temple-shrine order as an exercise of institutional power) independently supports the reading that the arrangement's coherence was administrative, not doctrinal. No corroboration exists from within the benefiting parties, whose own attestations insist the fusion was genuine — which is itself the signal this reading predicts.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70 at interval end) because the arrangement moved revenue, labor, and ritual obligation from villages and registered households to monastic centers while charging kami cults their independent standing as the price of protection. Suppression is higher still (0.78) because the binding mechanism was the enforced unaskability of the categorical question, backed from the seventeenth century by compulsory temple registration and censorship of kami-only reformers. Theater ratio climbs from 0.24 to 0.66: early glosses did real integrative work in a landscape that needed some answer, while by the late Edo period honji suijaku was recited as liturgy, defended by power, and increasingly disbelieved — performance substituting for doctrine. Accessibility collapse is moderate (0.52): exits existed (Ise-centered piety, Yoshida licensing, Pure Land congregations) but none offered ordinary communities a complete alternative ritual economy. Resistance (0.55) is episodic rather than constant — shrine riots, lawsuits, and a rising scholarly crescendo peaking near the interval end. The three series share one seven-point grid (1000-1868) so no metric borrows another's timeline; the terminal suppression value records the enforcement burden required at the moment of collapse, not enforcement capacity, which the edicts abolished overnight. Dynamics are monotonic-rising rather than cyclical: episodic resistance punctuates but does not reverse the accumulation. The vindicated propositions (honji suijaku, the dual Shinto gloss) are listed because the arrangement's operation nominally upheld them; on this reading they collected no rents and functioned as the theatrical layer the rising theater ratio measures.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the monastic seat the arrangement is a patrimony it built and administers: the categorical silence is prudence, the dues are support, the glosses are teaching. From the village and danka seats the same structure is a double levy with no doctrinal address — they pay both sides and may not ask what binds them. From the excluded Kokugaku seat the arrangement is visible whole, as corruption: precisely because they hold no seat, they can ask the question the seated cannot. The engine computes these divergences from power, exit, and role data; the divergence between the agenda-setter's coordination experience and the payers' extraction experience is the perspectival fact this story exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Monastic establishments sit nearest the beneficiary pole: they set terms, collect from both sides of every composite site, and hold arbitrage-grade repositioning within the landscape. Warrior administrators derive low directionality as beneficiaries of administrative peace, with a secondary agenda-setting interest that keeps them from the pure-beneficiary end. Embedded shrine clergy sit mildly beneficiary-side: protection and traffic received, autonomy paid. Village communities, danka households, and independent shrine lineages sit at or near the target pole — the first two because they fund both sides of every site, the third because lineage identity locks them into offices the composite devalued. Excluded reformers and Meiji authors carry no derived directionality from the standing arrangement; they are positioned outside it, which is analytically the point. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the arrangement as pure coordination (peaceful synthesis) erases the subordination rents — who paid, and that the categorical question stayed closed because answering it would have unwound the revenue map. Reading it as pure extraction (extraction wearing vestments) erases the real coordination delivered: two totalizing systems shared an archipelago for nine centuries without holy war, and villages received integrated ritual economies they could not have built alone. Tangled rope holds both: genuine coordination function, asymmetric extraction, active enforcement. The mandatrophy question resolves through the R5 interview rather than the flag: the founding problem (coexistence without annihilation) was live for most of the interval and is now dead — the arrangement was not outlived by its problem but executed alongside it — while the world-rearranges verdict confirms the arrangement carried real load. The mismatch consumer should find no zombie signature: founding status contested, verdict world_rearranges, gain_flow named, fixing_cost prohibitive — a captured, costly, load-bearing structure ended by exogenous force, which is what this reading says an incoherent bundle looks like from inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the shinbutsu coexistence kernel describes the operative pre-Meiji arrangement: ontological fusion (syncretic_fusion_reading), stable domain partition (domain_partition_reading), or an incoherent power-maintained bundle (this reading)?',
    'Analyze how actual jurisdictional and doctrinal disputes were settled across the interval — by appeal to a shared ontology, by stable boundary rules, or by institutional weight — using temple-shrine dispute records, shogunal adjudications, and doctrinal commentaries.',
    'If fusion holds, epsilon falls toward coordination-cost levels and the arrangement moves toward rope; if partition holds, the arrangement approaches a clean boundary-settlement with minimal ambiguity-extraction; this reading''s elevated epsilon and tangled_rope classification depend on incoherence being load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading-selection ambiguity within the shinbutsu kernel: this story is one of three competing instantiations.').

omega_variable(
    bunri_revelation_or_creation,
    'Did the Meiji separation edicts reveal a pre-existing incoherence, or did imposing categorical distinctions create the incoherence that then manifested as collapse?',
    'Compare pre-bunri internal records (did practitioners and institutions experience categorical tension before 1868?) with the geography and speed of post-edict disruption: rapid, low-resistance dissolution in regions with no prior separatist movement supports revelation; disruption tracking the imposed categories supports creation.',
    'If bunri created the incoherence, the standing arrangement''s authored extractiveness and theater values are overstated and the collapse reads as exogenous destruction of a functioning hierarchical coordination system rather than structural failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bunri_revelation_or_creation, empirical, 'Whether the 1868 collapse diagnosed or manufactured the bundle''s incoherence.').

omega_variable(
    extraction_attribution_boundary,
    'How much of the measured extraction belongs to the composite arrangement as such, versus to the surrounding medieval and Edo social hierarchies (estate extraction, bakufu status order) in which it was embedded?',
    'Compare extraction profiles of composite temple-shrine complexes against non-composite religious institutions (Ise, Yoshida-lineage shrines, Pure Land congregations) under comparable economic conditions.',
    'If most extraction is attributable to the embedding hierarchy, epsilon drops and the arrangement approaches coordination-with-context; if composite-specific extraction dominates (double obligations, subordination rents), the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_boundary, empirical, 'Attribution of extraction between the composite arrangement and its embedding social order.').

omega_variable(
    contingency_of_incoherence,
    'Was the bundle''s incoherence contingent on specific institutional choices (Tendai and Shingon capture of shrines, bakufu reliance on temple registration), or structurally necessary for any accommodation between a universal soteriology and autochthonous cults?',
    'Comparative history of religious syncretism under analogous conditions (Romano-Celtic, Afro-Caribbean, Central Asian Buddhist-indigenous fusions): do stable ontologies emerge where institutional incentives differ?',
    'If structurally necessary, the arrangement sits nearer a natural-limit profile (lower authored suppression, higher accessibility collapse); if contingent, it is a constructed constraint and the presence of identifiable beneficiaries becomes decisive for its classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingency_of_incoherence, conceptual, 'Whether the categorical silence was chosen or unavoidable.').

omega_variable(
    shrine_priest_position_ambiguity,
    'Were shrine priesthoods embedded in temple hierarchies net beneficiaries or net payers — did legitimating association outweigh subordination costs?',
    'Prosopographical study of embedded shrine lineages: income trajectories, autonomy losses, and litigation against parent temples across the interval.',
    'If net payers, the beneficiary set narrows to monastic and warrior seats and effective extraction concentrates further on the remaining payers; if net beneficiaries, the coordination side of the ledger strengthens and epsilon moderates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shrine_priest_position_ambiguity, empirical, 'Ambiguous structural position of the embedded shrine clergy seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1000, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_bundle_tr_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1000, 0.24).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1150, 0.31).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1300, 0.39).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1450, 0.45).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.51).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.59).
narrative_ontology:measurement(shinbutsu_bundle_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.66).

% Extraction over time
narrative_ontology:measurement(shinbutsu_bundle_be_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(shinbutsu_bundle_be_t1150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1150, 0.5).
narrative_ontology:measurement(shinbutsu_bundle_be_t1300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1300, 0.58).
narrative_ontology:measurement(shinbutsu_bundle_be_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1450, 0.61).
narrative_ontology:measurement(shinbutsu_bundle_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.64).
narrative_ontology:measurement(shinbutsu_bundle_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.67).
narrative_ontology:measurement(shinbutsu_bundle_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_bundle_su_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1000, 0.34).
narrative_ontology:measurement(shinbutsu_bundle_su_t1150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1150, 0.41).
narrative_ontology:measurement(shinbutsu_bundle_su_t1300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1300, 0.49).
narrative_ontology:measurement(shinbutsu_bundle_su_t1450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1450, 0.54).
narrative_ontology:measurement(shinbutsu_bundle_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.61).
narrative_ontology:measurement(shinbutsu_bundle_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.69).
narrative_ontology:measurement(shinbutsu_bundle_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' covers three structurally distinct claims about the same millennium of practice. This story carries the incoherent-bundle claim (no operative ontology; ambiguity load-bearing; high epsilon because the categorical silence was monetized). The syncretic-fusion sibling carries the ontological-unification claim (lower epsilon: a genuine shared commitment coordinates rather than extracts). The domain-partition sibling carries the stable-boundary claim (lowest epsilon: a clean jurisdictional settlement). They are linked because each reading's evidentiary base constrains the others: dispute-resolution records showing power settling what doctrine left open support this reading and undercut the other two. Per the epsilon-invariance principle these are separate constraints with separate epsilon values, not one constraint viewed through different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
