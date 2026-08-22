% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Te Tiriti Partnership Reading: Kāwanatanga-as-Governance with Retained Tino Rangatiratanga
 *   domain: constitutional/indigenous_rights
 *
 * SUMMARY:
 *   This constraint instantiates the rangatiratanga-retention reading of the
 *   contested Treaty of Waitangi / Te Tiriti o Waitangi kernel. Under the
 *   Māori text and the contra proferentem principle (ambiguity in a drafted
 *   instrument resolved against the drafter, here the Crown), kāwanatanga is
 *   read as a delegation of governance authority sufficient to administer a
 *   shared order, not a cession of sovereignty; tino rangatiratanga — full
 *   chiefly authority over lands, kāinga, and taonga — is expressly retained.
 *   The arrangement is therefore structurally a partnership: the Crown's
 *   governance authority is legitimately exercised only with hapū consent
 *   where it touches matters of rangatiratanga. Measured against this
 *   reading's own terms, the 19th and early 20th century record shows the
 *   Crown repeatedly acting as though kāwanatanga included what it did not —
 *   the Native Land Court's individualisation regime, the New Zealand
 *   Settlements Act 1863 confiscations, and unilateral legislative overrides
 *   of hapū authority. These are read here as the coordination structure
 *   exceeding its bounds, not as evidence the structure was never a
 *   coordination structure at all (that latter claim belongs to the sibling
 *   retrospective_snare_exposure reading, a separate constraint). Since 1975
 *   (Treaty of Waitangi Act, Waitangi Tribunal) the trend shown in the
 *   measurements reverses: extraction and suppression fall as consent-based
 *   mechanisms (settlements, co-governance, tribunal redress) are built out,
 *   restoring closer alignment with the reading's own account of what the
 *   Treaty always required.
 *
 * KEY AGENTS:
 *   - hapu_and_iwi: primary rights-holder under the retained rangatiratanga grant (organized/constrained) — beneficiary in principle, payer in unilateral-override periods
 *   - crown_when_acting_with_consent: administers kāwanatanga (institutional/mobile) — legitimate agenda-setter only within the partnership bound
 *   - land_alienated_claimants: bear the historical cost of Crown action exceeding kāwanatanga as this reading defines it (powerless/trapped)
 *   - waitangi_tribunal: analytical seat applying this reading (and others) to specific historical grievances
 *   - english_text_literalists: excluded by the contra proferentem move — their preferred text does not control here
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.58).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.62).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti Partnership Reading: Kāwanatanga-as-Governance with Retained Tino Rangatiratanga").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'eb2b3541-685d-4a06-a24c-3de074aa687e').
narrative_ontology:cs_kernel_codification('eb2b3541-685d-4a06-a24c-3de074aa687e', fixed_text).
narrative_ontology:cs_authority_grounding('eb2b3541-685d-4a06-a24c-3de074aa687e', practice).
narrative_ontology:cs_interpretation_layer_present('eb2b3541-685d-4a06-a24c-3de074aa687e').
narrative_ontology:cs_reading_relation('eb2b3541-685d-4a06-a24c-3de074aa687e', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('eb2b3541-685d-4a06-a24c-3de074aa687e', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('eb2b3541-685d-4a06-a24c-3de074aa687e', foundational, maori_text_controls_under_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_controls_under_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('eb2b3541-685d-4a06-a24c-3de074aa687e', maori_text_controls_under_contra_proferentem, conventional).
narrative_ontology:cs_axiom('eb2b3541-685d-4a06-a24c-3de074aa687e', foundational, kawanatanga_denotes_bounded_governance_not_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_denotes_bounded_governance_not_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('eb2b3541-685d-4a06-a24c-3de074aa687e', kawanatanga_denotes_bounded_governance_not_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('eb2b3541-685d-4a06-a24c-3de074aa687e', secondary, crown_authority_over_maori_domains_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(crown_authority_over_maori_domains_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('eb2b3541-685d-4a06-a24c-3de074aa687e', crown_authority_over_maori_domains_requires_ongoing_consent, deontological).
narrative_ontology:cs_reference_frame('eb2b3541-685d-4a06-a24c-3de074aa687e', partnership_framework_at_signing).
narrative_ontology:cs_drift_state('eb2b3541-685d-4a06-a24c-3de074aa687e', post_waitangi_tribunal_settlement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('eb2b3541-685d-4a06-a24c-3de074aa687e', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_under_unilateral_crown_action).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, land_alienated_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, settler_descendant_population).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed Te Tiriti in Māori, ceding kāwanatanga (governance authority over settlers and the management of a shared political order) while retaining tino rangatiratanga — full chiefly authority over lands, villages, and taonga. Under this reading they are the primary intended beneficiary of a partnership structure requiring their ongoing consent before the Crown's governance authority extends into their domains. Where the Crown has acted without that consent — legislative override, land purchase pressure, resource management without consultation — the same partnership structure becomes the mechanism through which costs are imposed on them, since the Crown's legitimacy claim is drawn from the very instrument that promised otherwise.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi, payer).

% Holds kāwanatanga — the delegated authority to govern, legislate for the general population, and administer a unified political order — but under this reading that authority is bounded: legitimate only insofar as it operates in partnership with, and with the consent of, hapū and iwi over matters touching tino rangatiratanga. When it observes this boundary (co-governance arrangements, Treaty settlements, consultation processes) it benefits from a stable, legitimated governance mandate it could not otherwise claim from the Māori text alone.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, agenda_setter,
    institutional, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, beneficiary).

% Descendants of hapū whose lands were purchased, confiscated, or legislated away in the 19th and 20th centuries, often through processes (Native Land Court individualisation, confiscation under the New Zealand Settlements Act, compulsory acquisition) that proceeded as though kāwanatanga included sovereignty over land title — a reading this constraint denies was ever ceded. They bear the retrospective cost of a Crown acting beyond its Treaty mandate as this reading defines it; redress through the Waitangi Tribunal is available but is partial, slow, and non-restorative of the original land base.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, land_alienated_claimants, payer,
    powerless, generational, trapped, national).

% Benefits from the stable governance order kāwanatanga was intended to establish — courts, currency, infrastructure, a single political community — without bearing the direct cost of the historical land alienation carried out under an overreading of that same authority. Has no formal role in Treaty partnership negotiations but is affected by their outcomes through settlement costs, co-governance arrangements, and shifts in resource allocation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_descendant_population, beneficiary,
    organized, generational, mobile, national).

% A standing commission of inquiry empowered to determine whether Crown acts or omissions are consistent with the principles of the Treaty — including, under this reading, whether kāwanatanga was exercised within its governance bounds or whether it improperly overrode retained tino rangatiratanga. Recommends redress but cannot itself compel restitution of land; its findings depend on the reading of the Treaty text it applies.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Historical and some contemporary legal actors who read the English text's 'sovereignty' as controlling and treat kāwanatanga as a translation of that full cession. This reading structurally excludes their premise — the contra proferentem doctrine applied here treats the Māori text as authoritative against the drafting party, so their preferred textual basis has no standing within this constraint's operation, though it persists as a rival account elsewhere.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, english_text_literalists, excluded,
    powerful, civilizational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bounded division of governing authority: the Crown administers a shared political and legal order (currency, courts, external relations, settler administration) while hapū and iwi retain authority over their own lands, resources, and internal affairs — with Crown authority over Māori domains exercised only through ongoing negotiated consent, not unilateral assertion.
% TRANSFER_FUNCTION: In its coordinating operation, the arrangement transfers governance capacity (kāwanatanga) from hapū to the Crown for matters of shared order, while nothing in tino rangatiratanga moves at all — it is retained, not transferred. Where the Crown has acted beyond that grant, the same instrument's legitimacy claim has permitted transfer of land, resource control, and legislative authority from hapū to the Crown and settler interests without the consent this reading holds is required.
% ABSENT_VOICES: Chiefs who signed the Māori text with tino rangatiratanga's plain meaning in mind — full chiefly authority — are gone, but their expressed intent is reconstructed through oral history, tribal record, and linguistic analysis rather than heard directly; where the Crown's post-signing conduct diverged from this understanding, no signatory hapū consented to that divergence at the time it occurred.
% DISAPPEARANCE_RATIONALE: If this reading's authority (that kāwanatanga is bounded governance and tino rangatiratanga is retained, requiring consent) were abandoned in favor of the crown-cession reading, the legal basis for co-governance structures, Treaty settlements, and Waitangi Tribunal findings against unilateral Crown action would collapse — decades of redress architecture and negotiated authority-sharing arrangements would lose their textual foundation.
% FOUNDING_PROBLEM: In 1840, competing colonial and Māori political orders needed a mechanism to coexist without the wholesale destruction or subjugation of hapū authority — a framework permitting settler governance to function while hapū retained control over their own lands, people, and customs.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the Crown by linguistic and historical scholarship on the Māori text (Ruth Ross, Claudia Orange), by hapū oral tradition transmitted independently of Crown record-keeping, and by the Waitangi Tribunal's own textual analyses in reports such as the Te Paparahi o Te Raki inquiry — all concluding independently that the Māori signatories understood themselves to be retaining rangatiratanga, not ceding it wholesale. The Crown's own historical administrative conduct (land purchasing operations, legislative overrides) is not independent corroboration of the founding problem being resolved; it is precisely the conduct this reading identifies as exceeding the Treaty's grant.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are authored on a historical arc, not as static values, because this reading's own account of the constraint's operation changes dramatically across the interval: near-zero at signing (1840), rising sharply through the confiscation and land-court era (1863-1900) as the Crown acted as though kāwanatanga extended into land title, and declining from the mid-1980s as Treaty settlement and co-governance mechanisms bring Crown conduct back toward the partnership structure this reading holds was always intended. The interval-end extractiveness (0.30) and suppression (0.35) are moderate rather than low because the underlying land alienation has not been reversed — settlements are financial and partial, not restorative of the original land base — so genuine ongoing cost persists even as the mechanism of imposition has softened.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū and iwi are the intended primary beneficiary of the coordination structure this reading describes — they retain the rangatiratanga that matters most to them (land, resource, cultural authority) in exchange for accepting a shared governance order. But where the Crown acted unilaterally, exactly the same textual instrument became the legitimating cover for extraction: the land_alienated_claimants group is a subset of hapu_and_iwi across time, split out here because their directional relationship to the constraint (high effective extraction, trapped exit, generational time horizon) is structurally distinct from the group's relationship to the constraint when the Crown observes consent. The Crown itself splits similarly: it is the beneficiary of a legitimated governance mandate only when it honours the partnership bound, and reverts to an agenda-setter imposing costs when it exceeds that bound.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coexistence of two political orders without hapū subjugation — is contested rather than resolved: the tribunal and reconciliation architecture built since 1975 addresses it partially, but the underlying land base has not been restored, so declaring the founding problem 'dead' would be premature. Classifying this as a rope (rather than collapsing it into either an unqualified natural-partnership story or an unqualified extraction story) allows the historical divergence between the Treaty's coordination promise and the Crown's mid-period conduct to register as a departure FROM the coordination function, rather than evidence the coordination function never existed — that latter claim is exactly what the sibling retrospective_snare_exposure reading makes as its own separate constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kawanatanga_semantic_scope,
    'Does ''kāwanatanga'' in the 1840 Māori text denote a delegation of governance authority bounded by retained tino rangatiratanga, or was it intended (or reasonably understood by at least some signatories) as a broader transfer that the English text''s ''sovereignty'' merely rendered more explicitly?',
    'Comparative linguistic and historical analysis of contemporaneous missionary usage of ''kāwanatanga'' and ''rangatiratanga,'' cross-referenced against hapū oral accounts of the signing and against Crown officials'' private correspondence describing intended scope.',
    'If kāwanatanga is found to have carried broader connotations for at least some hapū or officials at signing, the sharp partnership/bounded-governance reading this constraint authors weakens, and the constraint moves toward a more contested or mixed characterization rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_semantic_scope, empirical, 'Whether the Māori-text reading of kāwanatanga as bounded governance is the historically correct interpretation of the signatories'' shared understanding.').

omega_variable(
    reading_selection_and_kernel_disagreement,
    'Given that this constraint is one reading among three structurally distinct readings of the same kernel (crown_cession_reading, rangatiratanga_retention_reading, retrospective_snare_exposure), what determines which reading a given legal or political actor adopts, and does that selection itself track power rather than textual or historical merit?',
    'Track which reading is invoked by which institutional actor (courts, Tribunal, Crown negotiators, hapū claimants) across specific disputes, and whether reading-selection correlates with which reading favors that actor''s position in the particular dispute.',
    'If reading-selection tracks self-interest rather than independent textual analysis, the apparent ''rope'' character of this reading is itself contingent on which parties currently hold enough power to make it operative — a partnership reading enforced only when convenient to the Crown would collapse toward the tangled_rope or snare pole despite this story''s own ε being authored low-to-moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_disagreement, conceptual, 'Whether the choice among kernel readings is itself power-driven rather than merit-driven — the structural question this reading, by design, does not resolve.').

omega_variable(
    settlement_adequacy_as_restoration,
    'Do post-1985 Treaty settlements and co-governance arrangements actually restore the partnership balance this reading claims was intended, or do they formalize a permanently reduced rangatiratanga at a fraction of its original scope?',
    'Compare settlement quantum and returned land/resource control against pre-1840 hapū land holdings and authority scope; assess whether co-governance arrangements grant decision-making parity or advisory status only.',
    'If settlements are found to formalize permanent diminishment rather than restoration, the declining extractiveness trend authored in this story''s later measurements (2010, 2024) may be overstated — the mechanism would have shifted from confiscation to permanent partial extinguishment, which changes the terminal classification from rope toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settlement_adequacy_as_restoration, empirical, 'Whether the modern settlement regime resolves or merely formalizes the historical departure from partnership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1863, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1863, 0.3).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.55).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1950, 0.6).
narrative_ontology:measurement(trea_tr_t1985, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(trea_tr_t2010, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement(trea_be_t1863, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1863, 0.55).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(trea_be_t1985, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(trea_be_t2010, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.25).
narrative_ontology:measurement(trea_su_t1863, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1863, 0.7).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(trea_su_t1985, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(trea_su_t2010, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the Treaty of Waitangi's authority-transfer clause' per the ε-invariance principle: crown_cession_reading (English text controls; ε high, near-total cession, largely uncontested extraction from a Crown-authority standpoint), rangatiratanga_retention_reading (this story; Māori text controls; ε moderate, genuine coordination core with historical departures), and retrospective_snare_exposure (textual divergence itself is the extraction mechanism; ε very high, no genuine consent possible under mistranslation). The three do not share one ε because they are not the same constraint — each reading fixes a different beneficiary/victim structure and a different account of what was actually agreed. They are linked here rather than merged because courts, the Tribunal, and political actors invoke different readings in different disputes, and the contamination/coupling machinery should be able to trace how a shift in the operative reading in one venue creates pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
