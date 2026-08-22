% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Codified Hindu Family Law Authority (Hindu Marriage Act 1955, Civil-Court Interpretive Reading)
 *   domain: comparative law / constitutional pluralism / religious governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the marriage_authority_kernel: the
 *   claim that marriage and family authority for the Hindu community derives
 *   from democratically codified statute (Hindu Marriage Act 1955 and
 *   companion acts) as interpreted by civil courts. The standing arrangement
 *   under contest — the referent for epsilon — is that codified regime as it
 *   actually operates, assessed by this reading's own lights, never the
 *   secular code the reading gestures toward. The arrangement has a genuine
 *   coordination function: it replaced fragmented, often unenforceable
 *   scriptural and customary marriage governance with a single rulebook
 *   applied predictably by state courts, guaranteeing monogamy and formal
 *   maintenance and divorce rights. It simultaneously carries asymmetric
 *   extraction: gendered costs concentrate on women (restricted practical
 *   divorce access, Section 9 restitution-of-conjugal-rights decrees, weak
 *   maintenance enforcement, property-control gaps), interfaith couples are
 *   pushed outside the act's coverage entirely, and the state and reformist
 *   elites collect legitimacy from the codification project. The claim/metric
 *   gap is deliberate: the reading CLAIMS tangled_rope (both functions are
 *   real), and the metrics independently describe moderately extractive,
 *   actively enforced operation — the engine computes per-seat
 *   classifications from the structural data; nothing here reconciles claim
 *   to metrics. Sibling readings of the same kernel are separate constraints
 *   with their own victim sets and epsilon values; they appear here only as
 *   network links and omega content, never inside this constraint's
 *   classification.
 *
 * KEY AGENTS:
 *   - civil_judiciary: agenda-setting administrator (institutional/constrained) — interprets the code, adjudicates divorce and maintenance, and holds a family-law jurisdiction that depends on the act's continuation
 *   - union_legislature: agenda-setting co-administrator (institutional/constrained) — enacted and amends the code, owns the deferred uniform-code question, constrained by coalition politics
 *   - hindu_women: primary target with partial benefit (organized/trapped) — bear the gendered costs while holding formal monogamy, maintenance, and inheritance claims
 *   - hindu_male_spouses: principal material beneficiary with payer residue (moderate/constrained) — retain household property control and conjugal-rights leverage, bear monogamy and maintenance liability
 *   - hindu_reformist_elites: legitimacy beneficiary (organized/mobile) — the codification vindicated their modernization program; they can redirect advocacy toward the uniform-code campaign
 *   - interfaith_hindu_marriage_parties: excluded boundary cases (powerless/constrained) — outside the act's coverage unless one party converts; their exclusion polices the community boundary
 *   - customary_adjudicative_communities: displaced authorities (organized/regional) — caste and community councils removed from formal adjudication, persisting informally, partially preserved by statutory customary carve-outs
 *   - constitutional_scholars_and_commissions: analytical observer (organized/analytical) — law commissions and feminist jurisprudence documenting the gap between the uniformity claim and plural practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Codified Hindu Family Law Authority (Hindu Marriage Act 1955, Civil-Court Interpretive Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative law / constitutional pluralism / religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '70808752-2cfe-4465-ba3f-e39d541566ed').
narrative_ontology:cs_kernel_codification('70808752-2cfe-4465-ba3f-e39d541566ed', fixed_text).
narrative_ontology:cs_authority_grounding('70808752-2cfe-4465-ba3f-e39d541566ed', lineage).
narrative_ontology:cs_interpretation_layer_present('70808752-2cfe-4465-ba3f-e39d541566ed').
narrative_ontology:cs_reading_relation('70808752-2cfe-4465-ba3f-e39d541566ed', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('70808752-2cfe-4465-ba3f-e39d541566ed', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('70808752-2cfe-4465-ba3f-e39d541566ed', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('70808752-2cfe-4465-ba3f-e39d541566ed', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('70808752-2cfe-4465-ba3f-e39d541566ed', foundational, statutory_supremacy_over_scriptural_interpretation).
narrative_ontology:cs_axiom_status(statutory_supremacy_over_scriptural_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('70808752-2cfe-4465-ba3f-e39d541566ed', statutory_supremacy_over_scriptural_interpretation, conventional).
narrative_ontology:cs_axiom('70808752-2cfe-4465-ba3f-e39d541566ed', foundational, gradualist_path_to_uniform_code).
narrative_ontology:cs_axiom_status(gradualist_path_to_uniform_code, holdable).
narrative_ontology:cs_axiom_grounding('70808752-2cfe-4465-ba3f-e39d541566ed', gradualist_path_to_uniform_code, instrumental).
narrative_ontology:cs_reference_frame('70808752-2cfe-4465-ba3f-e39d541566ed', codified_hindu_law_uniformity).
narrative_ontology:cs_drift_state('70808752-2cfe-4465-ba3f-e39d541566ed', contemporary_ucc_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70808752-2cfe-4465-ba3f-e39d541566ed', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, union_legislature).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_reformist_elites).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_hindu_marriage_parties).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, customary_adjudicative_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_male_spouses).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_male_spouses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Hindu Marriage Act and companion statutes, adjudicates divorce, maintenance, and custody disputes, and develops binding case law that absorbs doctrinal drift between legislative amendments. Its family-law jurisdiction and professional identity are built on administering this code; ceding jurisdiction back to communal authorities would require legislative action the judiciary does not control.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Enacted the 1955 codification and holds the amendment power (used, for example, to liberalize divorce by mutual consent in 1976). It also owns the constitutionally deferred uniform-civil-code directive, which it has left open for decades because touching any community's marriage law carries coalition-breaking electoral cost. Its exit from the current settlement is blocked by the same politics that sustain it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, union_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Legal professionals and social-reform currents whose twentieth-century program — codify Hindu law, displace scriptural and customary arbiters — was vindicated by the 1955 acts. The codification is their legacy; they collect standing from it and can redirect their advocacy toward the uniform-code campaign if the current settlement loses legitimacy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_reformist_elites, beneficiary,
    organized, generational, mobile, national).

% Bear the arrangement's gendered costs: divorce legally available but practically slow, stigmatized, and economically punishing; restitution-of-conjugal-rights decrees enforceable against them; maintenance orders chronically under-enforced; household property control remaining predominantly with husbands and in-laws. They also hold the arrangement's formal protections — guaranteed monogamy, statutory maintenance and inheritance claims — and have organized advocacy (feminist litigation, law-reform campaigning) that keeps the settlement contested. Leaving the marriage is legally possible; leaving the arrangement's reach is not, since every marital dispute routes back through the same courts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_women, beneficiary).

% Retain predominant control of household property and family decision-making under the codified regime, and are the disproportionate beneficiaries of conjugal-rights enforcement and of maintenance-order under-enforcement. They bear the arrangement's costs on the other side: compulsory monogamy, alimony and maintenance liability on divorce, and loss of the unilateral divorce latitude customary practice sometimes afforded. Their position inside the arrangement is comfortable enough that they do not agitate to change it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_male_spouses, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_male_spouses, payer).

% Couples in which one partner is not Hindu fall outside the act's coverage entirely: they must either convert (bringing the marriage under this regime) or marry under the Special Marriage Act, whose public notice period exposes them to family and community opposition, sometimes violence. Their exclusion from this arrangement is what polices the community boundary the act administers; they have no seat in shaping the rules that determine their only lawful routes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_hindu_marriage_parties, excluded,
    powerless, biographical, constrained, national).

% Caste councils, khap-style assemblies, and community elders who adjudicated marriage disputes before codification and were displaced from formal authority by the act. They persist informally — regulating marriage choices, sometimes enforcing penalties — and retain partial statutory footholds through customary-divorce savings clauses that courts recognize when proven. Their formal exclusion is settled; their informal operation continues underneath the codified layer.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, customary_adjudicative_communities, excluded,
    organized, generational, constrained, regional).

% Law Commission consultations, academic family-law scholarship, and feminist jurisprudence that document the gap between the arrangement's uniformity and reform claims and its plural, gendered operation. They take testimony from every other seat, publish findings, and propose amendments, but hold no enforcement power over the settlement they analyze.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_scholars_and_commissions, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, hindu_male_spouses).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single codified rulebook for marriage validity, divorce grounds, maintenance, and adoption within the Hindu community, applied predictably by civil courts — solving the fragmentation and unenforceability of pre-1955 plural scriptural and customary governance.
% TRANSFER_FUNCTION: Moves adjudicative authority from dispersed customary and scriptural fora to state civil courts; moves marital bargaining power asymmetrically (formal monogamy and maintenance claims toward wives, property control and conjugal-rights leverage toward husbands); moves legitimacy to the state and to the reformist elites whose codification project the arrangement vindicates.
% ABSENT_VOICES: Women inside unhappy marriages at the codification moment had little voice — the advisory and drafting processes were dominated by male legislators and jurists. Interfaith couples were never in the conversation: the act's coverage boundary was drawn around them without their participation. Customary communities were not consulted on which of their practices would survive the savings clauses. All three groups would object to current terms if seated, and their objections surface only obliquely, through litigation brought after the fact.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, millions of pending and future marriage, divorce, and maintenance disputes would lose their governing framework; the courts' family-law dockets would collapse into jurisdictional vacuum; customary councils would reclaim adjudicative space unevenly across regions; and interfaith couples' route structure would destabilize. The world would rearrange immediately because a large institutional apparatus and a population-scale set of marital expectations are built on top of the code.
% FOUNDING_PROBLEM: Post-independence India inherited fragmented, often unenforceable Hindu marriage governance — plural scriptural interpretations, customary variation, no reliable divorce or maintenance machinery — and the Constituent Assembly committed to codifying it as the first stage of a projected move toward a uniform civil code.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship and successive Law Commission reports corroborate, from outside the benefiting parties, that the original fragmentation problem was real and is now substantially solved; feminist jurists and commission consultations equally corroborate that the arrangement's operative justification has shifted from solving that problem to maintaining institutional settlement and legitimacy. The benefiting seats (judiciary, legislature, reformist elites) attest the problem is still live in security-of-rights terms. No single external attestation settles the status — hence contested.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial but not confiscatory — the arrangement transfers real bargaining power and property control asymmetrically while also delivering enforceable rights that many payers value. Suppression is authored at 0.60 as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope): the arrangement suppresses rival adjudicative authorities and channels interfaith couples into costly routes, but it does not suppress exit from marriage itself, which is legally available though socially and economically punishing. Theater_ratio 0.30: the adjudicative machinery does real work at volume, but a growing share of the arrangement's legitimating activity defends the uniformity and gradual-reform claims that practice increasingly contradicts. Accessibility_collapse 0.52: alternatives exist (Special Marriage Act, customary routes) but are socially costly, procedurally exposed, or judicially uncertain, so they collapse only partly. Resistance 0.50: sustained feminist legal scholarship, law-commission debate, and periodic amendment pressure meet an entrenched institutional settlement. The measurement series run on ONE shared time grid (points 0-70, decade steps) with every tracked metric authored at every point. The suppression_requirement series traces a real enforcement-capacity arc, which is why it is tracked at all: high initial enforcement effort to displace customary adjudication, decay through the middle decades as court operation normalized, then renewed intensification as boundary-policing of interfaith marriage and conversion became the active enforcement front. Extractiveness rises through the first three decades as the gap between formal rights and substantive outcomes widens faster than amendments close it, then plateaus; theater rises monotonically as the uniformity and gradualism claims age.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the civil_judiciary seat the arrangement is orderly codified governance it competently administers; from the hindu_women seat it is formal rights wrapped around substantive shortfall — divorce accessible in doctrine, punishing in practice; from the hindu_male_spouses seat it is a mild constraint (monogamy, maintenance liability) wrapped around preserved property control; from the hindu_reformist_elites seat it is a completed historical achievement; from the customary_adjudicative_communities seat it is dispossession with partial statutory preservation. Inter-institutionally, judiciary and legislature share the agenda-setter role but experience the constraint differently: courts absorb doctrinal drift through interpretation while the legislature owns the politically frozen amendment path — the same constraint is fluid for one institution and immovable for the other. Identity-lock dynamics appear on two seats: reformist elites are professionally fused with the codification achievement (their legacy is the 1955 moment, making criticism of the arrangement feel like self-repudiation), and customary authorities are institutionally fused with their displaced role (their standing exists as what the courts replaced). If either identity frame broke — elites accepting the arrangement as incomplete, customary bodies seeking formal re-integration — the coalition structure sustaining the current settlement would loosen.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. civil_judiciary, union_legislature, and hindu_reformist_elites sit near the beneficiary end (low d): the arrangement subsidizes their jurisdiction, legitimacy, and vindication respectively, and none bears its costs. hindu_women are declared victims with a secondary beneficiary position: the derivation places them near the target end (high d) — trapped exit amplifies this — while the secondary_role records that monogamy guarantees and formal maintenance claims partially offset, keeping them short of full-target. hindu_male_spouses are declared beneficiaries with a secondary payer position: low d with upward correction from their monogamy and maintenance liabilities. interfaith_hindu_marriage_parties and customary_adjudicative_communities are excluded rather than coordinated: they sit largely outside the transfer itself, but their exclusion is the enforcement object that maintains the community boundary, so they register as targets of the arrangement's boundary-maintenance function rather than as participants in its exchange. No directionality overrides are used: the role declarations plus exit options already encode the dual-positioned seats, and a per-power-atom override would be too coarse to distinguish the organized-power agents (women's collectives, reformist elites, customary councils) who hold genuinely different directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, frequently unenforceable marriage governance with no reliable divorce or maintenance machinery — was substantially solved by the codification, and the solution was real: courts process the docket, rights are enforceable, and the pre-1955 free-for-all did not return. The arrangement therefore cannot be dismissed as pure extraction, and the tangled_rope classification prevents that mislabeling. Equally, the classification prevents the opposite error: reading the surviving gendered cost structure as the necessary price of coordination. The mandatrophy question concentrates in the legitimating frame rather than the machinery: the uniformity claim and the gradualist promise age toward performance while the adjudicative function stays live, which is why theater_ratio rises while extractiveness plateaus. The arrangement is not yet a piton — no seat could change it cheaply, the payer side is mobilized enough to keep the structure contested, and the agenda-setters still collect enough legitimacy to maintain it actively — but the omega on gradualist-promise viability marks the exact seam along which it would decay into one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the marriage_authority_kernel (reading_id: hindu_codified_reading); how would instantiating a sibling reading — muslim_shariat_reading, christian_canonical_reading, parsi_communal_reading, or secular_civil_reading — change the victim set, the beneficiary structure, and epsilon?',
    'Cross-reading comparison across the five sibling stories: align victim sets, beneficiary seats, and extraction metrics, then test the expected structural delta (this reading moderate on gender equity — better than the shariat reading, worse than the secular reading) as a corpus-level prediction.',
    'The victim-set composition and epsilon are reading-indexed, not topic-indexed: a sibling instantiation redistributes who counts as harmed and by how much. Any cross-community comparison of ''personal law extraction'' that pools these stories without the reading index is invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: reading-contingent classification of a contested marriage-authority kernel.').

omega_variable(
    extraction_attribution_ambiguity,
    'Are the gendered costs measured under this arrangement authored by the codified framework itself, or inherited from the customary and scriptural practice the framework displaced but did not erase?',
    'Counterfactual comparison against pre-1955 customary adjudication records and against communities where customary marriage routes remain operative alongside the statute; isolate costs that appear only under the codified regime (e.g., Section 9 conjugal-rights decrees) from costs that predate it.',
    'If most measured costs are inherited, the constraint sits nearer the rope end (genuine coordination carrying residual custom); if the framework authors or amplifies them, it sits nearer the snare end of the tangled-rope range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_ambiguity, empirical, 'Attribution of measured extraction between the codified framework and the custom it displaced.').

omega_variable(
    customary_recognition_scope,
    'How extensively do the statutory customary carve-outs (Section 29(2) customary-divorce recognition and analogous savings clauses) undermine the uniformity claim that legitimates the arrangement?',
    'Survey of customary-divorce prevalence and rates of judicial recognition across regions and communities; measure the gap between the claimed uniform coverage and actual plural operation.',
    'Wide customary scope converts the uniformity axiom into partial performance, raising theater_ratio for the legitimating claim specifically and supporting drift hypotheses toward piton for the uniformity frame while the adjudicative machinery stays functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_recognition_scope, empirical, 'Scope of customary exceptions hollowing the uniformity claim.').

omega_variable(
    gradualist_promise_viability,
    'Does the gradualist axiom — codification as a stage toward an eventual uniform civil code under Article 44 — remain a live organizing commitment of this reading, or has it become cover for indefinite deferral?',
    'Track legislative action on the uniform civil code and amendment activity across the interval; test whether reform energy continues, stalls, or is redirected into symbolic commission reports.',
    'If the promise is cover, the reading''s instrumental grounding decays: the arrangement''s persistence shifts from transitional justification to inertia plus legitimacy collection, which is mandatrophy-relevant and would push the founding-problem status toward dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gradualist_promise_viability, conceptual, 'Viability of the gradualist trajectory claim that distinguishes this reading from both communal and secular siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Indian personal law' decomposes, per the epsilon-invariance principle, into five structurally distinct authority arrangements — one per reading of the marriage_authority_kernel — each with its own epsilon, beneficiary/victim structure, and enforcement profile. Pooling them under one label would force a single epsilon onto observables that yield different values (e.g., gender-cost profiles differ sharply between the shariat and codified-Hindu regimes). This story is the template case: the codified-Hindu precedent is repeatedly cited in uniform-civil-code debate as evidence that codification works, which makes this reading an upstream structural influence on the secular_civil_reading's operating environment. All five family members are linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
