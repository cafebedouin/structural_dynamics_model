% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist-Egalitarian Reading of the Dharmic Corpus
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   Since the constitutional turn, an operative interpretive regime governs
 *   the transmitted Vedic-dharmic corpus: authoritative textual meaning is
 *   whatever conforms to constitutional equality principles, caste hierarchy
 *   is treated as historical accretion laid over the texts rather than their
 *   essence, and rational critique supersedes birth-derived authority. The
 *   regime is carried by courts, statutes, curricula, and movement pressure,
 *   and it displaces a centuries-old hereditary interpretive monopoly. KEY
 *   AGENTS (by structural relationship): - dalit_emancipation_movements:
 *   primary beneficiary (organized/constrained) — claims equal standing
 *   inside the tradition; - reformist_interpretive_scholars: secondary
 *   beneficiary (organized/mobile) — staffs the new interpretive
 *   establishment; - constitutional_state_apparatus: agenda-setter and
 *   beneficiary (institutional/constrained) — enforces the frame and collects
 *   adjudication authority; - hereditary_priestly_lineages: primary target
 *   (organized/identity_locked) — bears loss of hereditary authority; -
 *   orthodox_lay_traditionalists: target (moderate/identity_locked) — bears
 *   delegitimation of inherited practice; - intermediate_caste_communities:
 *   mixed beneficiary/target (organized/constrained); -
 *   dalit_women_organizers: absent voice (powerless/trapped); -
 *   comparative_religion_scholars: analytical observer. The epsilon referent
 *   is the standing arrangement under contest — the operative reformist
 *   interpretive regime as actually enforced — assessed by this reading's own
 *   lights; it is never the fully realized egalitarian order the reading
 *   endorses. The claim (tangled_rope) and the metrics were authored
 *   independently: the claim states what I believe is structurally true of
 *   the arrangement, the metrics what I believe is descriptively true of its
 *   operation.
 *
 * KEY AGENTS:
 *   - dalit_emancipation_movements: primary beneficiary (organized/constrained) — gains standing and access inside the tradition
 *   - reformist_interpretive_scholars: secondary beneficiary (organized/mobile) — occupies the vacated interpretive ground
 *   - constitutional_state_apparatus: agenda-setter with beneficiary position (institutional/constrained) — enforces the frame, collects adjudication authority
 *   - hereditary_priestly_lineages: primary target (organized/identity_locked) — bears dispossession of hereditary authority
 *   - orthodox_lay_traditionalists: target (moderate/identity_locked) — bears delegitimation of inherited practice
 *   - intermediate_caste_communities: dual-positioned (organized/constrained) — gains mobility, loses local advantage
 *   - dalit_women_organizers: excluded voice (powerless/trapped) — subordinated in both venues
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — outside record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.52).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist-Egalitarian Reading of the Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '1767a4e3-0bed-4ef9-8b87-9649ca48816d').
narrative_ontology:cs_kernel_codification('1767a4e3-0bed-4ef9-8b87-9649ca48816d', fixed_text).
narrative_ontology:cs_authority_grounding('1767a4e3-0bed-4ef9-8b87-9649ca48816d', expertise).
narrative_ontology:cs_interpretation_layer_present('1767a4e3-0bed-4ef9-8b87-9649ca48816d').
narrative_ontology:cs_reading_relation('1767a4e3-0bed-4ef9-8b87-9649ca48816d', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('1767a4e3-0bed-4ef9-8b87-9649ca48816d', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('1767a4e3-0bed-4ef9-8b87-9649ca48816d', foundational, textual_meaning_conforms_to_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_conforms_to_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('1767a4e3-0bed-4ef9-8b87-9649ca48816d', textual_meaning_conforms_to_constitutional_equality, conventional).
narrative_ontology:cs_axiom('1767a4e3-0bed-4ef9-8b87-9649ca48816d', foundational, caste_hierarchy_is_historical_accretion).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('1767a4e3-0bed-4ef9-8b87-9649ca48816d', caste_hierarchy_is_historical_accretion, empirically_contingent).
narrative_ontology:cs_axiom('1767a4e3-0bed-4ef9-8b87-9649ca48816d', secondary, rational_critique_supersedes_birth_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_birth_authority, holdable).
narrative_ontology:cs_axiom_grounding('1767a4e3-0bed-4ef9-8b87-9649ca48816d', rational_critique_supersedes_birth_authority, instrumental).
narrative_ontology:cs_reference_frame('1767a4e3-0bed-4ef9-8b87-9649ca48816d', constitutionally_scrutinized_canon).
narrative_ontology:cs_drift_state('1767a4e3-0bed-4ef9-8b87-9649ca48816d', contemporary_india, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1767a4e3-0bed-4ef9-8b87-9649ca48816d', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_emancipation_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_interpretive_scholars).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, intermediate_caste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_lay_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, intermediate_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize campaigns for temple entry, equal ritual standing, and access to scriptural education. Their claims carry weight because the governing interpretive frame treats them as legitimate participants in the tradition rather than outsiders. Individual members sometimes exit through conversion, but the movements as such operate inside the tradition's field and have no comparable arena in which their claims would be heard.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_emancipation_movements, beneficiary,
    organized, generational, constrained, national).

% Hold university chairs, produce critical editions, and advise courts, commissions, and ministries on what the texts can mean. Their professional standing rose as the older interpretive monopoly receded, and they staff the committees that certify curricula and heritage policy. Working outside the frame would cost them their institutional position, though their credentials remain portable across adjacent fields.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_interpretive_scholars, beneficiary,
    organized, biographical, mobile, national).

% Courts, law commissions, and education ministries adjudicate disputes over ritual access, personal law, and curriculum content. The interpretive frame gives the state a workable rule for deciding which religious claims are enforceable, and decades of jurisprudence now rest on it. Abandoning the frame would unsettle the state's own founding legitimacy commitments, so it administers rather than revisits them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus, beneficiary).

% Families whose ritual office and teaching authority passed by birth. Temple-entry rulings, priest appointment reforms, and curriculum changes have removed much of the deference, income, and adjudicative role their office once commanded. Their authority claim is inseparable from the genealogies and doctrinal readings now ruled out of bounds; renouncing the claim would dissolve the office itself, so they defend it through litigation, counter-institutions, and quiet retention of domestic practice.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages, payer,
    organized, generational, identity_locked, national).

% Households who organize religious life around birth-based ritual roles and inherited community hierarchies. Open defense of those arrangements now carries legal exposure and social cost, so observance continues privately or in semi-autonomous institutions while public speech shifts to safer formulations. Their inherited identity is bound up with the arrangements being delegitimated.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_lay_traditionalists, payer,
    moderate, biographical, identity_locked, regional).

% Non-Brahmin cultivating and trading castes positioned between the old apex and the excluded base. They gained ritual mobility, administrative recognition, and educational access under the new frame, while losing some locally advantageous standings they held under the older settlement. Their position mixes gain and loss depending on which local hierarchy is being measured.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, intermediate_caste_communities, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, intermediate_caste_communities, payer).

% Women in Dalit neighborhoods who carry the compounded burden of caste and gender. Movement platforms center caste dignity under predominantly male leadership; feminist forums center gender and frequently miss caste. Their specific grievances rarely set the agenda in either venue, and they lack an institutional seat where the compound claim would be primary.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_women_organizers, excluded,
    powerless, generational, trapped, national).

% Academic students of South Asian religion who trace how the corpus is read under competing regimes of authority. They publish analyses of every interpretive camp, take no side in the contest, and provide the outside record against which each camp's self-description can be checked.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, comparative_religion_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a hierarchical scriptural inheritance and a constitutional egalitarian polity can share one population: it supplies a single interpretive frame under which formerly excluded castes can claim equal standing inside the tradition, the state can adjudicate religious practice by rule of law, and the corpus remains usable as living heritage rather than being abandoned or abolished.
% TRANSFER_FUNCTION: Moves interpretive authority — with the status, income, and institutional control attached to it — from hereditary priestly lineages to credentialed scholars, courts, and movement leadership; moves ritual access and public standing toward formerly excluded castes; moves adjudication of sacred meaning into state and academic institutions.
% ABSENT_VOICES: Dalit women organizers would object that both venues subordinate their compound grievance; they are present demographically but absent from agenda-setting. Rural ritual specialists without institutional affiliation have no seat at all. Orthodox voices are present but in sanitized form — the strongest traditionalist positions are legally and socially unspeakable in mainstream forums, so the recorded debate samples a moderated version of the opposition.
% DISAPPEARANCE_RATIONALE: If the frame vanished overnight, temple-entry settlements, the interpretive basis of caste-discrimination law, and the movements' claims to stand inside the tradition would all need refounding; hereditary authority claims would re-enter the field without the constitutional counterweight; curricula, personal-law codes, and heritage policy would require wholesale renegotiation.
% FOUNDING_PROBLEM: How can a population carry a hierarchical scriptural inheritance into a constitutional democracy founded on equality — specifically, how to dismantle caste-based exclusion (temple bans, ritual pollution rules, educational bars) without either abolishing the tradition wholesale or conceding its hierarchy as permanent.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox institutions themselves attest the conflict is live, from the opposite direction — they contest the frame's intrusions continuously. Independent corroboration outside both benefiting and paying camps: United Nations treaty-body reviews, longitudinal sociological survey series documenting continuing caste discrimination, and court-docket records of atrocity and access litigation. No camp attests the founding problem is dead.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.45 because the arrangement pairs a real, functioning coordination achievement — legal personhood for the excluded, temple access, textual admission — with a real transfer: interpretive authority, income, and status move off hereditary lineages onto credentialed and state seats, and the rate of that transfer is set by the winners of the transition, not negotiated symmetrically. Suppression is 0.52: enforcement runs through statutory machinery, court rulings, and curriculum control, plus a social-stigma layer that makes open traditionalist defense costly; but private observance, counter-institutions, and dissenting scholarship persist, so alternatives are pressured, not erased. Suppression is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope. Theater_ratio is 0.26 and rising: symbolic inclusion ceremonies, commemorative appointments, and heritage pageantry grow as the substantive frontier narrows. Accessibility_collapse is 0.35 — understanding the frame does not collapse the alternatives, since traditionalist readings remain practicable and legible. Resistance is 0.6 — orthodox contestation is organized, litigious, and durable. The temporal series run on one shared grid (t=0..100 at steps of 20) with every tracked metric authored at every point: base_extractiveness accumulates gradually as the new interpretive establishment consolidates; suppression_requirement traces the deliberate build-out of enforcement capacity from movement-era suasion to mature legal machinery, plateauing late; theater_ratio climbs as performative maintenance layers onto a still-functional core. The trajectory is monotonic drift, not cyclical, so no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the hereditary-priestly seat the arrangement is enforced dispossession: an authority constituted by birth and lineage is being dismantled by courts and committees that the lineage never consented to, and identity-lock makes exit equivalent to self-dissolution. From the Dalit-movement seat the same structure is long-delayed admission, and its enforcement machinery is the guarantee that admission holds. From the state seat it is routine jurisprudence — the ordinary administration of founding commitments. Inter-institutionally, the state and the religious establishments hold different time horizons (generational constitutional continuity versus lineage continuity) and different exit conditions, which is why identical rulings read as housekeeping in one seat and expropriation in the other. Among same-level actors, orthodox laity and intermediate castes hold comparable social power but different exits: the laity's identity-lock binds them to the contested arrangements, while intermediate castes can reposition within the new frame, which is why the same rulings stabilize one group's grievance and dissolve the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the movement, scholar, and state seats; the state's dual agenda-setter/beneficiary position keeps it near the subsidized end while acknowledging it also bears administrative cost. Victim declarations drive high directionality for the priestly lineages and orthodox laity, amplified toward the full-target end by identity_locked exit — their relationship to the arrangement is not contractual but constitutive, so no arbitrage softens their position. Intermediate castes sit mid-scale through their dual beneficiary/payer declaration: gains in mobility offset by losses in local standing. The excluded and observer seats feed no directionality — absence and analysis collect nothing and pay nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a hierarchical scriptural inheritance with constitutional equality — is still live, corroborated from outside every camp, so the arrangement has not outlived its mandate and mandatrophy_resolved is not declared. The analysis guards against two opposite mislabels. Reading the arrangement as pure liberation (a clean rope) erases the asymmetric transfer: someone coordinated, someone pays, and the same courts that admit the excluded strip the lineage. Reading it as pure extraction (a snare) erases the genuine coordination delivered: temple entry happened, legal personhood happened, and the movements' gains are not cover for anything. The live risk this story flags for future re-authoring is replacement-elite recursion (see omega replacement_elite_recursion): if the credentialed establishment hardens entry barriers mirroring the ones it dismantled, extraction migrates to a new axis and the arrangement begins operating as the thing it displaced — at which point the founding problem's status would need re-examination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story classifies one reading (reformist_egalitarian) of the kernel vedic_dharmic_corpus; do the sibling readings (hereditary_monopoly_reading, bhakti_devotional_reading) classify differently, and does any verdict transfer to the kernel as a whole?',
    'Compile the sibling stories and compare per-reading classifications; treat the kernel as a constraint family with per-member verdicts rather than a single label.',
    'If the hereditary sibling computes as heavily extractive and the bhakti sibling as lightly extractive, the corpus-level picture is a family of divergent constraints; collapsing them into one verdict would erase the indexical structure this framework exists to register.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'One-reading-of-kernel status: classification is indexed to this reading and does not automatically transfer across readings.').

omega_variable(
    authority_grounding_disagreement_location,
    'Where exactly do the three readings disagree — is the dispute located in the authority-grounding premise (birth versus devotion versus rational-constitutional critique), in the essence-or-accretion status of varna hierarchy, or in both?',
    'Structural comparison of the readings'' foundational axioms: identify the minimal set of premises such that altering one converts a holder of one reading into a holder of another.',
    'If the load-bearing premise is the authority source, mediation differs from the case where it is the textual-status claim; the foreclosure relation this reading holds toward the hereditary sibling depends on which premise is doing the contradicting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_disagreement_location, conceptual, 'Locates the structural site of inter-reading disagreement within the kernel contest.').

omega_variable(
    state_enforcement_dependence,
    'Does the reformist interpretive regime survive without state enforcement, or is its persistence dependent on courts, statutes, and curricula?',
    'Compare periods and jurisdictions with varying enforcement intensity; test whether voluntary uptake tracks or lags enforcement withdrawal.',
    'If enforcement-dependent, the pressure borne by orthodox seats exceeds what voluntary-consent measures suggest, and the regime''s stability is hostage to state capacity rather than settled consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_dependence, empirical, 'Enforcement dependence of the reformist interpretive regime.').

omega_variable(
    replacement_elite_recursion,
    'Will the new interpretive establishment — credentialed scholars, court-interpreters, movement bureaucracies — harden into a replacement orthodoxy with its own entry barriers?',
    'Track credential requirements, citation gatekeeping, and succession rules inside reformist institutions over time; compare against the entry-barrier profile of the displaced hereditary elite.',
    'If recursion occurs, the transfer migrates onto a new axis and the arrangement begins operating as the structure it displaced, shifting the appropriate classification for future re-authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_elite_recursion, empirical, 'Whether reformist elites reproduce the monopoly structure they dismantled.').

omega_variable(
    accretion_thesis_empirical_status,
    'Is the claim that caste hierarchy is historical accretion rather than scriptural essence philologically and historically robust, or does it depend on selective canonical weighting?',
    'Independent textual-critical review of stratum dating, manuscript evidence, and reception history, conducted outside both the movement and orthodox camps.',
    'If the thesis weakens, this reading''s foundational empirical axiom loses warrant and its foreclosure relation to the hereditary sibling destabilizes; if robust, the reading''s claim strengthens against revival pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accretion_thesis_empirical_status, empirical, 'Empirical robustness of the accretion thesis underpinning this reading.').

omega_variable(
    suppression_mechanism_composition,
    'Is the suppression bearing on traditionalist expression primarily structural (legal liability, institutional exclusion) or internalized (social stigma rendering open defense unspeakable even where legal risk is low)?',
    'Observation after enforcement changes: if traditionalist public speech revives where legal risk drops but stigma persists, the internalized component dominates; if speech stays quiet, the structural component dominates.',
    'If largely internalized, relaxing enforcement will not restore open contestation, and the measured resistance figure understates latent orthodox sentiment — the arrangement would look more consensual than it is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of suppression on traditionalist expression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_reformist_reading_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedic_reformist_reading_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(vedic_reformist_reading_tr_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(vedic_reformist_reading_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(vedic_reformist_reading_tr_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 80, 0.23).
narrative_ontology:measurement(vedic_reformist_reading_tr_t100, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 100, 0.26).

% Extraction over time
narrative_ontology:measurement(vedic_reformist_reading_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vedic_reformist_reading_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(vedic_reformist_reading_be_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(vedic_reformist_reading_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(vedic_reformist_reading_be_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(vedic_reformist_reading_be_t100, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedic_reformist_reading_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vedic_reformist_reading_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(vedic_reformist_reading_su_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(vedic_reformist_reading_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(vedic_reformist_reading_su_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(vedic_reformist_reading_su_t100, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the tradition's position on caste' covers three structurally distinct interpretive regimes instantiated from one kernel (vedic_dharmic_corpus): hereditary-monopoly, bhakti-devotional, and reformist-egalitarian. Each has its own epsilon, its own beneficiary/victim structure, and its own enforcement mode. Per the epsilon-invariance principle they are separate constraint stories linked through network.affects_constraints, not one constraint with a measurement parameter. This file authors only the reformist-egalitarian reading; the upstream hereditary reading historically supplied the authority claims this reading displaces, and the bhakti reading supplies the internal-tradition precedent for anti-hierarchical access that this reading cites as evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
