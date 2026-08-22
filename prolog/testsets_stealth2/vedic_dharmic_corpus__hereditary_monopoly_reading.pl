% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Ritual and Interpretive Authority (Vedic-Dharmic Corpus, Hereditary-Monopoly Reading)
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   A hereditary priestly order holds exclusive authority to recite, teach,
 *   and interpret the Vedic corpus and to officiate at the rites through
 *   which households mark birth, marriage, and death. Access to the corpus is
 *   gated at initiation, which orthodox practice reserves to males born into
 *   the priestly lineages; the social ordering that places the lineages at
 *   its apex is presented as divinely ordained in the founding verses and
 *   prescribed in the legal-didactic texts. Enforcement runs through temple
 *   and monastic estate control, the ritual economy of fees and endowments,
 *   initiation eligibility, and assembly recognition of who may speak for the
 *   tradition. The arrangement solves a real problem, namely
 *   multi-generational preservation of an enormous orally transmitted corpus
 *   and standardization of ritual practice across a subcontinent, while
 *   channeling fees, endowments, labor obligations, and interpretive
 *   supremacy to the lineage holders and excluding lower orders and women
 *   from the corpus and its authority. Claimed type and metrics are authored
 *   independently: this story claims tangled_rope because a genuine
 *   transmission-and-standardization function coexists with birth-gated
 *   asymmetric extraction requiring active enforcement; the metrics describe
 *   substantially extractive operation and are not tuned to the claim.
 *
 * KEY AGENTS:
 *   - - brahmin_priestly_lineages: Agenda-setting beneficiary (institutional/identity_locked) — administers the gate and collects through it
 *   - - temple_and_matha_estates: Secondary beneficiary (institutional/constrained) — endowment income rides on officiation exclusivity
 *   - - kshatriya_ruling_elites: Patron-beneficiary (powerful/constrained) — buys legitimation and pays heavily for it
 *   - - vaishya_mercantile_communities: Net payer (organized/constrained) — funds the order, receives ritual certification
 *   - - shudra_service_castes: Primary target (powerless/trapped) — bears service obligations and rite fees
 *   - - dalit_outcaste_communities: Primary target (powerless/trapped) — bears exclusion and pollution liability
 *   - - orthodox_household_women: Target (powerless/identity_locked) — barred from recitation and independent officiation
 *   - - non_brahmin_scripture_seekers: Excluded voice (organized/constrained) — barred from initiation and assembly recognition
 *   - - constitutional_equality_authorities: Analytical observer (institutional/analytical) — investigates and legislates at the margin
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.52).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Ritual and Interpretive Authority (Vedic-Dharmic Corpus, Hereditary-Monopoly Reading)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/political/social").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'dde9b146-dade-45ac-945f-993c7d4eba72').
narrative_ontology:cs_kernel_codification('dde9b146-dade-45ac-945f-993c7d4eba72', fixed_text).
narrative_ontology:cs_authority_grounding('dde9b146-dade-45ac-945f-993c7d4eba72', lineage).
narrative_ontology:cs_interpretation_layer_present('dde9b146-dade-45ac-945f-993c7d4eba72').
narrative_ontology:cs_reading_relation('dde9b146-dade-45ac-945f-993c7d4eba72', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('dde9b146-dade-45ac-945f-993c7d4eba72', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('dde9b146-dade-45ac-945f-993c7d4eba72', foundational, varna_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('dde9b146-dade-45ac-945f-993c7d4eba72', varna_divinely_ordained, theological).
narrative_ontology:cs_axiom('dde9b146-dade-45ac-945f-993c7d4eba72', foundational, hereditary_transmission_necessary).
narrative_ontology:cs_axiom_status(hereditary_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('dde9b146-dade-45ac-945f-993c7d4eba72', hereditary_transmission_necessary, instrumental).
narrative_ontology:cs_reference_frame('dde9b146-dade-45ac-945f-993c7d4eba72', varna_divine_birth_order).
narrative_ontology:cs_drift_state('dde9b146-dade-45ac-945f-993c7d4eba72', post_constitutional_equality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dde9b146-dade-45ac-945f-993c7d4eba72', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_and_matha_estates).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_service_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, orthodox_household_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_mercantile_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_mercantile_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite, teach, and preserve the corpus; officiate at the rites marking birth, marriage, and death; decide through initiation eligibility and assembly recognition who may study the texts and how they may be read. Receive fees, honoraria, land grants, and endowments for these offices. Leaving would mean renouncing the lineage identity, training, and standing that constitute who they are; there is no version of exit that keeps the office.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, beneficiary).

% Hold endowed land, manage festival calendars, employ ritual staff, and collect offerings. Their income depends on the exclusivity of who may officiate; opening officiation to uninitiated competitors would strand endowed assets and hereditary offices. They persist across centuries and administer the day-to-day machinery through which eligibility rules are applied.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_and_matha_estates, beneficiary,
    institutional, civilizational, constrained, continental).

% Patronize courts, fund large sacrifices, and endow temples; in return they receive consecration and public legitimation of their rule as part of a divinely sanctioned ordering in which they hold the second rank. The payments are heavy, but repudiating the ordering would undermine the sanctity of their own office, so they defend the arrangement that taxes them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites, payer).

% Fund temple construction, sponsor rites, and pay recurring ritual fees; in return they receive ritual certification of commercial respectability, auspicious dating for transactions, and the standing markers their trade networks read as signals of trustworthiness. Skipping the fees means losing the markers; there is no rival certifier their partners would accept.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_mercantile_communities, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_mercantile_communities, beneficiary).

% Barred from Vedic study and from most officiating roles; obligated to render service to the higher orders; required to pay the lineage holders for the weddings, funerals, and rites no household can skip. Their caste is ascribed at birth and inherited by their children; leaving would sever family, village standing, and marriage networks simultaneously, with no recognized place elsewhere.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_service_castes, payer,
    powerless, generational, trapped, continental).

% Placed outside the purity ordering entirely: denied temple entry, denied ritual service, frequently denied access to village commons and water. They carry pollution liabilities assigned by the order's rules and pay for whatever limited rites they are permitted. Exit means leaving every local tie behind, and the surrounding society recognizes no destination that is not itself ordered.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, continental).

% In orthodox practice they are excluded from Vedic recitation and from independent officiation; they conduct household ritual under male lineage supervision and transmit observance to children. Their standing as daughters, wives, and mothers is defined by fidelity to the order, so the hierarchy is carried inside kinship itself; exit entails leaving kin and community altogether, not merely changing a role.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, orthodox_household_women, payer,
    powerless, biographical, identity_locked, regional).

% Would study, copy, memorize, and interpret the corpus and dispute its meaning in assembly. Initiation eligibility bars them from the curriculum, and assembly recognition bars their readings from counting as the tradition's voice. Their objections circulate only at the margins of the institutions that would have to admit them for the objections to matter.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scripture_seekers, excluded,
    organized, biographical, constrained, continental).

% Modern state bodies that take testimony on caste discrimination, commission sociological studies, legislate temple-entry and succession reforms, and litigate the boundary between religious institution and civil right. They investigate and occasionally intervene from outside; they do not officiate within the ritual order and their findings bind it only through law.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, constitutional_equality_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits an enormous orally composed corpus across generations with high fidelity, standardizes ritual procedure across a subcontinent so that each rite is performed correctly once rather than improvised per household, and provides a single adjudicating line for disputes about duty, law, and textual meaning.
% TRANSFER_FUNCTION: Moves ritual fees, land grants, temple endowments, service labor, and public deference from lower-order communities and ruling patrons to the priestly lineages and their temple estates; moves interpretive authority exclusively upward to the initiated.
% ABSENT_VOICES: Non-brahmin scripture seekers, women barred from recitation, and outcaste communities would object that access to the corpus and its authority is priced at birth. They sit outside the assembly entirely: ineligible for initiation, unrecognized as interpreters, and in the outcaste case denied temple presence. The order's self-understanding is unanimous partly because the seats that would dissent were never admitted to the room.
% DISAPPEARANCE_RATIONALE: Temple finance, life-cycle rites, marriage alliances, village labor obligations, and festival calendars all presuppose the lineage gate. Overnight removal would reopen officiation to whoever could demonstrate competence, redirect endowment income away from hereditary offices, and force every household to renegotiate who speaks at its rites and on what terms.
% FOUNDING_PROBLEM: Before widespread writing, an enormous corpus existed only as memorized sound held in living memory; preserving it across generations demanded full-time specialists bound to the work from childhood, and dispersed communities needed a single standard for correct ritual performance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: philological and historical scholarship documents that manuscript, print, and digital transmission preserved the corpus without the birth gate, and that the ordering operated prescriptively in periods when it contradicted observed social practice; anti-caste reform traditions and constitutional jurisprudence attest the arrangement's exclusionary costs from outside the order. Orthodox lineages themselves attest that the transmission problem was once live. No corroborating source outside the beneficiary set attests that the birth gate remains necessary for transmission; that claim is attested only by the lineages that staff it.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: fees, endowments, service obligations, and deference flow to the lineage seats decoupled from the marginal cost of the rites performed, and access to the corpus itself is priced at birth. Suppression 0.52 (end-state): the structural bars of initiation eligibility, temple entry, and assembly recognition remain, but their legal enforcement has eroded; the temporal series shows the enforcement ratchet building under court and endowment patronage (peak 0.72) and then decaying under codified law and reform legislation, which is why the series is authored rather than left to the static scalar. Theater_ratio 0.42: recitation and officiation continue at scale after manuscript, print, and digital transmission removed the scarcity that made hereditary specialists necessary, and a growing share of ceremonial activity defends the hierarchy rather than transmits the corpus; the ratio rises steadily but stays short of the proxy-domination threshold. Accessibility_collapse 0.55: within the order's framework, alternatives such as lay study and non-lineage officiants are foreclosed, but exit routes through heterodox participation, conversion, and migration remained costly-yet-real throughout, so alternatives never fully collapsed. Resistance 0.6: sustained heterodox devotional currents, anti-caste movements, and mass conversion episodes; individually powerless seats mattered chiefly through coalition, which is where their historical leverage came from. Identity-lock: the lineage seats fuse professional function, relational standing, and cosmological self-concept, making exit unthinkable from inside even where formally available; the same fusion binds orthodox household women to the order through kinship identity. All three temporal series run on one shared seven-point grid so no metric borrows another's end-state value.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat experiences the arrangement as custodianship it was born into and maintains at real cost of discipline; the trapped payer seats experience the same structure as a ceiling on speech, study, and ritual agency they never consented to and cannot leave; the patron seat experiences an exchange it judges fair because it purchases sanctity for its own rule. Same-level divergence: vaishya mercantile communities and non-brahmin scripture seekers hold comparable organized standing, but the seekers' exclusion from initiation strips them of the exit-by-recognition that mercantile wealth partially buys, so constraint-specific factors rather than global power differentiate their positions. Inter-institutionally, the temple estates and the modern state authorities relate to the same eligibility rules as asset-protection and as discrimination respectively. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The lineage seats and temple estates are declared beneficiaries and derive low directionality on the subsidy side; the three declared victim groups derive high directionality, amplified by trapped or identity-locked exit. Two overrides correct dual-role derivations the single-role chain cannot express: kshatriya ruling elites appear as net beneficiaries because legitimation received exceeds tribute paid, but they hold no entry in the beneficiary or victim arrays, so the derivation would fall back to a canonical default rather than read their exchange position; they are overridden to d=0.28 at power=powerful. Vaishya mercantile communities pay recurring fees yet receive ritual certification their trade networks price, a near-symmetric position; they are overridden to d=0.60 at power=organized. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both faces visible: calling the arrangement a pure snare would erase the genuine transmission achievement that originally justified the lineages' position, and calling it a rope would erase the birth gate that prices access and the enforcement that maintains it. Mandatrophy is not declared resolved: the founding problem of corpus preservation is contested rather than dead, since transmission continues but no longer requires the gate, and the world-rearranges verdict confirms the arrangement still organizes marriage, labor, temple finance, and ritual timing. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie flag; the live risk signal instead sits in the theater_ratio series, which climbs toward the proxy-domination threshold as transmission necessity atrophies. If that ratio crosses 0.5 while the founding problem completes its migration to print and digital media, the residual function becomes performance and the classification should be revisited toward the degraded-inertial type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_location,
    'This constraint instantiates the hereditary_monopoly_reading of the vedic_dharmic_corpus kernel; which structural elements do the sibling readings relocate, and where exactly does the disagreement sit?',
    'Read the sibling story files vedic_dharmic_corpus__bhakti_devotional_reading and vedic_dharmic_corpus__reformist_egalitarian_reading and diff their beneficiary/victim arrays and axioms against this file.',
    'Under the devotional sibling the birth gate dissolves and the beneficiary set collapses to voluntary teaching roles; under the egalitarian sibling interpretive authority migrates to constitutional-rational criteria and the lineage seats lose agenda-setting power entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_location, conceptual, 'Committer structure: one of three readings of one kernel; disagreement located in the source-of-authority premise (birth versus devotion versus constitutional reason).').

omega_variable(
    divine_ordination_vs_construction,
    'Is the varna ordering a discovered cosmic constitution, as this reading asserts, or a constructed social arrangement whose ordination framing benefits identifiable hereditary officeholders?',
    'Comparative philology and reception history of the varna verses: trace whether the ordering operated as description of observed social reality or as prescription imposed against countervailing evidence, and test whether the claim''s force depends on the officeholders'' enforcement.',
    'If constructed, the divine-ordination framing functions as a false summit, a naturality claim shielding an enforced arrangement, and the constraint''s classification shifts decisively toward the extractive types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordination_vs_construction, conceptual, 'Naturality ambiguity: cosmic order versus constructed hierarchy with identifiable beneficiaries.').

omega_variable(
    apaurusheyatva_epistemic_insulation,
    'Does the doctrine that the corpus is uncreated and authorless function as epistemic insulation that removes the arrangement''s textual foundations from critical revision?',
    'Examine the interpretive tradition''s handling of internal contradictions and counter-textual evidence: whether disconfirming readings are absorbed, deferred, or ruled inadmissible by appeal to unauthoredness.',
    'If insulating, part of the measured suppression is doctrinal rather than coercive: enforcement succeeds by making critique a category error rather than by punishing critics, so effective suppression exceeds observable sanction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apaurusheyatva_epistemic_insulation, empirical, 'Whether the authorlessness doctrine insulates the kernel from revision.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression holding the arrangement in place structural (initiation bars, temple exclusion, economic dependency on ritual employment) or internalized (karmic acceptance of station as deserved)?',
    'Post-exit trajectory of communities that left the order through conversion or migration: if station-acceptance persists after the structural bars are removed, a substantial share is internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests, targets carry the hierarchy with them after exit, and removing formal bars alone would not collapse the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in caste-religious enforcement.').

omega_variable(
    transmission_function_separability,
    'Is accurate multi-generational transmission of the corpus separable from birth-restricted access to it?',
    'Natural experiment: corpus fidelity under manuscript, print, and digital transmission regimes with open access; compare recitation-error rates and interpretive continuity against lineage-only transmission.',
    'If separable, the access restriction is extraction riding on a real coordination function rather than its price; if inseparable, part of the measured extraction is the cost of the transmission itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_function_separability, empirical, 'Whether the coordination function requires the birth gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 60, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Vedic dharmic authority' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle: the hereditary-monopoly reading (this file), the bhakti devotional reading, and the reformist egalitarian reading. Each carries its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observer-dependent. This file authors epsilon only for the birth-gated arrangement as the hereditary-monopoly reading assesses it. The upstream reading (this one) historically influenced the operating environment of the devotional currents through enforcement, and the egalitarian reading arose downstream as a repudiation; the family links encode those dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, powerful, 0.28).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
