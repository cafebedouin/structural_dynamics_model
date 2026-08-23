% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Covenant Transmission Boundary
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   This story classifies ONE reading of the abrahamic_covenant kernel: the
 *   reading that Genesis 17:19-21 transmits the eternal covenant exclusively
 *   through Isaac, excluding Ishmael's line. As an operating arrangement,
 *   that reading functions as a religious identity boundary: it determines
 *   membership, allocates ritual obligation, sustains communal continuity
 *   across two millennia of dispersion, and concurrently denies standing to
 *   every rival descent or succession claim. The boundary has a genuine
 *   coordination function — bounded identity, obligation allocation,
 *   intergenerational transmission — entangled with asymmetric costs borne by
 *   parties who never consented to the boundary: Ishmaelite claimants and the
 *   later Islamic succession tradition. ASSUMPTIONS: the interval maps time
 *   units to centuries (T=0 approximates c. 450 BCE, the Ezra-Nehemiah
 *   genealogy reforms; T=25 approximates c. 2050 CE, so the final time point
 *   is partly projected). The claim and the metrics are independent authored
 *   facts: claimed_type records the structure I believe true (tangled_rope);
 *   the metrics describe how the arrangement actually operates.
 *
 * KEY AGENTS:
 *   - - rabbinic_authority: Primary agenda-setter (institutional/identity_locked) — administers the boundary and collects the interpretive jurisdiction that flows from sole adjudicatorship
 *   - - covenantal_israelite_community: Primary beneficiary (organized/constrained) — receives covenant identity and continuity, bears the obligation and endogamy load (dual-positioned)
 *   - - proselyte_adherents: Secondary beneficiary (moderate/identity_locked) — enters through the administered gate on the authority's terms
 *   - - ishmaelite_claimants: Primary target (powerless/trapped) — descent claims rendered void, no consenting forum
 *   - - muslim_prophetic_succession_tradition: Secondary target (institutional/trapped) — succession claim denied standing regardless of its scale
 *   - - contemporary_interfaith_theologians: Excluded voice (moderate/mobile) — argues inclusively outside adjudication
 *   - - comparative_religion_scholars: Analytical observer (analytical/analytical) — sees the full boundary mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.7).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.48).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Covenant Transmission Boundary").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '4ea06442-607a-4713-bf1d-030dc5269227').
narrative_ontology:cs_kernel_codification('4ea06442-607a-4713-bf1d-030dc5269227', fixed_text).
narrative_ontology:cs_authority_grounding('4ea06442-607a-4713-bf1d-030dc5269227', lineage).
narrative_ontology:cs_interpretation_layer_present('4ea06442-607a-4713-bf1d-030dc5269227').
narrative_ontology:cs_reading_relation('4ea06442-607a-4713-bf1d-030dc5269227', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ea06442-607a-4713-bf1d-030dc5269227', abrahamic_covenant__christian_supersessionist_reading, influences).
narrative_ontology:cs_axiom('4ea06442-607a-4713-bf1d-030dc5269227', foundational, covenant_eternally_located_in_isaac_line).
narrative_ontology:cs_axiom_status(covenant_eternally_located_in_isaac_line, holdable).
narrative_ontology:cs_axiom_grounding('4ea06442-607a-4713-bf1d-030dc5269227', covenant_eternally_located_in_isaac_line, theological).
narrative_ontology:cs_axiom('4ea06442-607a-4713-bf1d-030dc5269227', secondary, ishmael_nationally_blessed_outside_berit).
narrative_ontology:cs_axiom_status(ishmael_nationally_blessed_outside_berit, holdable).
narrative_ontology:cs_axiom_grounding('4ea06442-607a-4713-bf1d-030dc5269227', ishmael_nationally_blessed_outside_berit, theological).
narrative_ontology:cs_reference_frame('4ea06442-607a-4713-bf1d-030dc5269227', exclusive_isaacite_election).
narrative_ontology:cs_drift_state('4ea06442-607a-4713-bf1d-030dc5269227', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4ea06442-607a-4713-bf1d-030dc5269227', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, covenantal_israelite_community).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, proselyte_adherents).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, muslim_prophetic_succession_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, covenantal_israelite_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Genesis 17 and administers everything that follows from the exclusive reading: who counts as covenant-born, who may convert, whose marriage and descent claims stand. Holds the deference, jurisdiction, and livelihood that attach to being the sole authorized adjudicator of covenant membership. Stepping away from the role would mean abandoning the textual mastery and communal standing that constitute the role itself.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Born into or admitted to covenant membership; receives identity, election-status, liturgy, and continuity across dispersal. Carries the corresponding load: circumcision, commandment observance, endogamy pressure, and the standing duty to transmit the distinction to children. Exit exists in assimilation or secular identity, but costs community, family ties, and inherited meaning.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, covenantal_israelite_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, covenantal_israelite_community, payer).

% Outsiders who join through the conversion process the adjudicating authority administers, accepting its terms and obligations. Gain full membership and belonging, and remain dependent on that authority's continued recognition, having surrendered a prior identity to enter.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, proselyte_adherents, beneficiary,
    moderate, biographical, identity_locked, global).

% Persons and communities tracing descent or spiritual inheritance through Abraham's firstborn son. Under the exclusive reading their genealogical and devotional claims carry no standing inside the covenantal frame: acknowledged as nations, not as heirs. They never consented to the boundary that excludes them, and no forum exists in which their claim is heard.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, excluded).

% The later religious civilization claiming the Abrahamic covenant continues through Ishmael and culminates in Muhammad. The exclusive reading denies that succession any place in the covenant's economy, making recognition structurally unavailable regardless of the tradition's size, antiquity, or scholarly depth.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, muslim_prophetic_succession_tradition, payer,
    institutional, civilizational, trapped, global).

% Jewish, Muslim, and Christian scholars who argue for inclusive or plural readings of the Abraham narratives. They publish, convene, and petition, but hold no seat in the halakhic processes that determine membership; their arguments circulate without adjudicative force.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, contemporary_interfaith_theologians, excluded,
    moderate, generational, mobile, global).

% Academic observers of the three traditions and their mutual exclusions. Neither collect nor bear anything under the arrangement; they map its textual basis, its historical consolidation, and its effects on intercommunal relations.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and maintains a bounded covenantal community across exile and dispersion: settles who belongs, allocates ritual obligations, secures intergenerational transmission of identity, and coordinates the community's self-understanding against assimilation and rival descent claims.
% TRANSFER_FUNCTION: Moves covenantal recognition and heir-status away from Ishmaelite lines (and any claim outside Isaac's line), concentrating election-status in the Isaac-descended community; moves interpretive jurisdiction over membership to the adjudicating authority; moves observance-obligations onto members and converts.
% ABSENT_VOICES: Ishmaelite and Muslim claimants were absent from every council where the reading was fixed and where it is maintained — from the post-exilic assembly through the talmudic academies to contemporary batei din. Interfaith theologians argue inclusively but outside adjudication. Part of the tradition's interior unanimity reflects these absences.
% DISAPPEARANCE_RATIONALE: If the exclusive transmission claim vanished overnight, Jewish membership law would reorganize around descent and conversion criteria no longer anchored to a single line, the Who-is-a-Jew disputes would lose their textual anchor, rabbinic jurisdiction over marriage and conversion would lose its warrant, and the Islamic succession claim would meet no doctrinal denial — the identity architecture of all three Abrahamic communities would rearrange.
% FOUNDING_PROBLEM: After the Babylonian exile, the restored Judean community faced a membership crisis: displaced populations, intermarriage, disputed genealogies, and competing claims to inheritance of the ancestral promises. The community needed a determinate criterion for who counted as covenant Israel; Genesis 17's Isaac-focus supplied the textual warrant for a bounded, line-defined people.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Persian-period Yehud, working from Elephantine papyri, the genealogy lists in Ezra-Nehemiah, and contemporaneous imperial resettlement policy, attest the post-exilic membership crisis independently of the tradition's own self-description; Samaritan and Christian sources preserve parallel accounts of lineage disputes without collecting anything from the rabbinic arrangement. No source outside the benefiting parties attests that the founding problem required permanent exclusivity — that further step is attested only by the tradition's own texts.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70): the arrangement transfers heir-status and recognition away from unconsenting outsider lineages while concentrating adjudicatory jurisdiction in the administering authority; insider obligations are substantially offset by belonging, so the asymmetry falls chiefly on the excluded. Suppression is moderate (0.48) and bimodal in the standing arrangement: statutory enforcement where the adjudicating authority holds state power over marriage and conversion, voluntary-communal enforcement in the diaspora; suppression is authored as a raw structural property and is not scaled by power or scope — the engine owns any scaling. Theater ratio is low-moderate (0.30): boundary maintenance is mostly functional (membership decisions, conversions, marriage adjudication), with a performative component in purity-of-descent rhetoric and public boundary display. Accessibility_collapse is 0.45: alternatives persist and remain livable — the rival lineage reading operates as a living civilization, reform movements broke with descent exclusivity internally, and secular exit exists — understanding the boundary does not erase the alternatives. Resistance is 0.62: fourteen centuries of counter-tradition, internal reform rupture, and sustained interfaith critique. CYCLICAL NOTE: the suppression_requirement series is deliberately non-monotonic — enforcement capacity oscillated with political sovereignty (Persian-period coercive backing, Roman-era decline, medieval kehillah self-governance rebound, emancipation-era erosion, modern partial re-statification). The oscillation is a side effect of external political conditions, not an intermittent-reinforcement mechanism. All three tracked series share one time grid ({0,5,10,15,20,25}); the final point carries a projected basis.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the administering authority's seat the boundary is fidelity to a received text and the guarantor of communal survival; from the insider member's seat it is belonging purchased with obligation; from the Ishmaelite and Islamic seats the identical structure is a denial issued without consent and without appeal; from the analytical seat it is a membership mechanism whose costs fall outside its constituency. The engine computes this divergence from power, exit, and directionality data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The administering authority declares as beneficiary and sits at the near-full-beneficiary end (d near 0.05): it runs the boundary and collects the jurisdiction, deference, and livelihood attached to running it. Insider members are declared beneficiary with a genuine secondary payer position — the structural derivation would read beneficiary-plus-constrained-exit as strongly subsidized, but the obligation load, endogamy cost, and transmission duty pull them materially toward symmetric; a directionality override sets organized-power agents to d=0.30 to capture the dual position. Proselytes derive near the beneficiary end: they voluntarily accepted the terms and gained membership. Ishmaelite claimants and the Islamic succession tradition are declared victims with no exit from a denial imposed from outside; both sit near the full-target end (d near 0.95), the Islamic tradition's institutional power doing nothing to soften a denial it cannot reach. Interfaith theologians and scholars sit outside the transfer entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-exilic membership determination — is still live: Who-is-a-Jew disputes, conversion standards, and descent controversies are active, so there is no mandatrophy resolution and no sunset to declare; the arrangement is not a piton candidate. Classification matters here in both directions: reading the arrangement as a pure coordination rope would erase the unconsenting outsider lineages who bear its asymmetric costs; reading it as a pure snare would erase the real identity-coordination work that has carried a dispersed community for twenty-five centuries. The tangled_rope claim holds both halves open for the engine to weigh.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (isaac_covenant_reading) of the kernel abrahamic_covenant; what would change structurally if the sibling ishmael_covenant_reading were adopted?',
    'Comparative structural analysis of the two readings'' beneficiary/victim sets and enforcement requirements: the sibling relocates the covenant locus to Ishmael''s line, which dissolves this reading''s victim set, creates a mirrored exclusion of Isaac-line claims, and reassigns adjudicatory authority to a different interpretive tradition.',
    'Under the sibling reading this constraint''s entire extraction topology inverts: the excluded become the included, the boundary''s epsilon attaches to a different victim set, and the classification computed here would not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling adoption inverts the victim set.').

omega_variable(
    genesis17_exclusivity_scope,
    'Does Genesis 17 itself assert exclusivity (Ishmael outside the covenant) or primacy-with-inclusion (v.21 fixes the covenant in Isaac while v.20 extensively blesses Ishmael and promises him nationhood)?',
    'Philological analysis of the berit terminology and the chapter''s internal structure: whether ''my covenant I will establish with Isaac'' excludes Ishmael from covenant-standing altogether or merely designates the line of the eternal sign, with Ishmael blessed under a lesser inclusion.',
    'If the text supports primacy-with-inclusion, the exclusive reading is an interpretive construction layered on a more inclusive kernel, epsilon drops materially, and the arrangement trends toward rope; if exclusivity is textually robust, the reading is faithful transmission and the measured extraction is inherent to the commitment itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genesis17_exclusivity_scope, conceptual, 'Whether the anchor text compels the exclusive reading or permits an inclusive sibling.').

omega_variable(
    victim_set_temporal_extension,
    'Does the victim set legitimately extend to the later Islamic succession tradition, or does Islamic-tradition injury belong to the sibling ishmael_covenant_reading''s own contest rather than to this reading''s standing arrangement?',
    'Counterfactual isolation analysis: ask whether this reading''s denial imposes costs on Islamic succession claims even if no rival reading ever existed — the denial of standing issues from this reading''s exclusivity alone, but its felt injury presupposes a contender claim.',
    'If injury requires the rival claim, part of the victim cost is jointly produced by the kernel contest and should be attributed across both stories; if the denial alone suffices, the victim declaration stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_temporal_extension, conceptual, 'Whether later Islamic tradition belongs in this reading''s victim set or only in the kernel-level contest.').

omega_variable(
    enforcement_capacity_bimodality,
    'Is the standing arrangement''s suppression better measured by the statutory enforcement arm (state-backed jurisdiction over marriage and conversion in Israel) or by the diaspora''s voluntary-communal enforcement?',
    'Jurisdiction-disaggregated enforcement data: compare sanction availability, case volumes, and exit costs under statutory versus communal enforcement regimes.',
    'A statutory-weighted measure raises effective suppression toward the historical range and strengthens enforcement-dependence findings; a diaspora-weighted measure lowers it and shifts weight onto identity-retention mechanisms instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_bimodality, empirical, 'Which enforcement regime dominates the standing suppression profile.').

omega_variable(
    divine_decree_vs_construction,
    'Is the Isaac-exclusive boundary a discovered feature of a divinely decreed election (as the tradition asserts — a mountain-like given) or a constructed institutional boundary maintained by identifiable beneficiaries?',
    'Not empirically resolvable from inside the framework: the tradition''s own epistemology treats the decree as given, while comparative-historical analysis shows the boundary consolidating through post-exilic institutional needs and rabbinic adjudication. Resolution would require a prior commitment on revelatory epistemology itself.',
    'If treated as construction, emerges_naturally stays false and the FSM-style question (who profits from presenting a choice as decree) remains live; if treated as decree, the boundary''s persistence requires no beneficiary explanation and the extraction analysis narrows to effects-on-outsiders only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_decree_vs_construction, conceptual, 'Natural-law versus constructed-constraint ambiguity in the boundary''s ultimate ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(abra_tr_t5, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(abra_tr_t10, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(abra_tr_t15, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(abra_tr_t25, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(abra_be_t5, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(abra_be_t10, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(abra_be_t15, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(abra_be_t25, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 25, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(abra_su_t5, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(abra_su_t10, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(abra_su_t15, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(abra_su_t25, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Abrahamic covenant' per the epsilon-invariance principle: the label conflates at least four structurally distinct claims — locus of covenant transmission (THIS story: exclusive through Isaac, high extraction on excluded lineages), rival transmission through Ishmael culminating in Muhammad (separate story, inverted victim set), successor-community identity (christian_supersessionist_reading: covenant relocated to the church), and the territorial grant (land_promise_constraint: conditionality/fulfillment dispute with distinct geopolitical stakes). Each carries its own epsilon, stakeholders, and classification; forcing one story to span them would make epsilon observer-dependent. Edges run from this story to its siblings because the exclusive claim is the upstream position the others define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
