% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Normative Authority Structure — Abolitionist Reading (Standing Arrangement)
 *   domain: religious/normative/legal
 *
 * SUMMARY:
 *   The standing arrangement under contest is the Dharmasastra-grounded
 *   normative authority structure: a fixed textual corpus (dharmasutras,
 *   smritis, nibandha commentaries) transmitted through Brahmin lineages,
 *   claiming final authority over ritual, law, marriage, inheritance, and
 *   social rank, and chartering the varna-jati hierarchy with its apex
 *   benefits and base burdens. This story authors that arrangement through
 *   the abolitionist reading's lights: the framework is fundamentally
 *   oppressive, its authority is irredeemable, and the remedy is wholesale
 *   abandonment rather than reinterpretation. Epsilon's referent is the
 *   standing arrangement itself as this reading assesses it — never the
 *   egalitarian order the reading would build in its place. Per the
 *   epsilon-invariance principle, the colloquial label 'Dharmasastra'
 *   decomposes into three linked constraint stories: this abolitionist
 *   reading (epsilon 0.72 for the standing arrangement), the
 *   orthodox_literalist reading (low epsilon: eternal duty, minimal
 *   extraction), and the reformist_contextual reading (intermediate epsilon:
 *   time-bound accretions extractive, ethical core exempt). Each is a
 *   separate constraint with its own beneficiaries, victims, and
 *   classification; the family links run through network.affects_constraints.
 *   KEY AGENTS (by structural relationship): - brahmin_ritual_elites: Agenda
 *   setter (institutional/identity_locked) — authors, transmits, interprets
 *   the corpus; collects its distinctive rents - landholding_dominant_castes:
 *   Primary beneficiary (powerful/constrained) — receives labor, deference,
 *   and ratification of property - upper_caste_households: Secondary
 *   beneficiary and payer (moderate/identity_locked) — gains graded status,
 *   pays ritual fees, reproduces endogamy - shudra_service_castes: Payer
 *   (moderate/constrained) — hereditary labor and service obligations,
 *   historically barred from literacy - dalit_bahujan_communities: Primary
 *   target (powerless/constrained) — bears untouchability, polluted labor,
 *   exclusion, violence - caste_oppressed_women: Payer
 *   (powerless/identity_locked) — bears the corpus's gender provisions
 *   layered on caste rank - anti_caste_movements: Excluded voice turned
 *   contestant (organized/mobile) — barred from the interpretive conversation
 *   for centuries, now contests from outside its channels -
 *   constitutional_state: Analytical observer (institutional/analytical) —
 *   adjudicates collisions between corpus-derived claims and fundamental
 *   rights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.72).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.46).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.72).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Normative Authority Structure — Abolitionist Reading (Standing Arrangement)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious/normative/legal").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '10518fd9-7da1-4d47-b204-6dc85957f066').
narrative_ontology:cs_kernel_codification('10518fd9-7da1-4d47-b204-6dc85957f066', fixed_text).
narrative_ontology:cs_authority_grounding('10518fd9-7da1-4d47-b204-6dc85957f066', extraction).
narrative_ontology:cs_interpretation_layer_present('10518fd9-7da1-4d47-b204-6dc85957f066').
narrative_ontology:cs_reading_relation('10518fd9-7da1-4d47-b204-6dc85957f066', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('10518fd9-7da1-4d47-b204-6dc85957f066', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('10518fd9-7da1-4d47-b204-6dc85957f066', foundational, textual_authority_wholly_illegitimate).
narrative_ontology:cs_axiom_status(textual_authority_wholly_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('10518fd9-7da1-4d47-b204-6dc85957f066', textual_authority_wholly_illegitimate, deontological).
narrative_ontology:cs_axiom('10518fd9-7da1-4d47-b204-6dc85957f066', foundational, hierarchy_constitutive_not_accretional).
narrative_ontology:cs_axiom_status(hierarchy_constitutive_not_accretional, holdable).
narrative_ontology:cs_axiom_grounding('10518fd9-7da1-4d47-b204-6dc85957f066', hierarchy_constitutive_not_accretional, empirically_contingent).
narrative_ontology:cs_reference_frame('10518fd9-7da1-4d47-b204-6dc85957f066', varna_hierarchy_charter).
narrative_ontology:cs_drift_state('10518fd9-7da1-4d47-b204-6dc85957f066', contemporary_post_ambedkarite, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('10518fd9-7da1-4d47-b204-6dc85957f066', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmin_ritual_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, landholding_dominant_castes).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_caste_households).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_bahujan_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_service_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, caste_oppressed_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, upper_caste_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose, memorize, transmit, and interpret the corpus; train successors in Sanskrit and ritual procedure; officiate rites for patron households; collect dakshina and honoraria; adjudicate disputed points of practice. Their standing, livelihood, and lineage identity rest entirely on the framework's continued authority; abandoning it would dissolve the social meaning of a lifetime's training.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmin_ritual_elites, agenda_setter,
    institutional, generational, identity_locked, continental).

% Dominant peasant and merchant castes whose control of land, credit, and village office was historically ratified by the corpus's allocations of duty and honor. They receive deference, labor, and marriage-alliance advantage under the arrangement and fund the temples and priests that reproduce it. Economic adaptation is possible for them, but their customary ratification lapses if the framework's authority does.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, landholding_dominant_castes, beneficiary,
    powerful, generational, constrained, continental).

% Twice-born households that reproduce the arrangement daily through endogamous marriage, ritual patronage, and domestic purity practice. They receive graded status relative to those below while paying ritual fees to those above — gaining and paying inside the same structure. Exit would mean breaking the kinship networks that constitute their social world.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_caste_households, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, upper_caste_households, payer).

% Laboring and artisan castes bound by hereditary service expectations and historically barred from Vedic learning. They supply agricultural, craft, and ritual-service labor upward and received protection and feast-cycle reciprocity in return; they were long denied the literacy that would let them read the rules governing them. Schooling and constitutional rights have widened their options, but village-level sanctions still constrain them.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_service_castes, payer,
    moderate, biographical, constrained, continental).

% Communities placed outside the varna order altogether, assigned polluting hereditary tasks such as scavenging, leatherwork, and corpse handling, denied temple entry, well access, and schooling, and subject to violence when transgressing boundary rules. Conversion to Buddhism, Islam, or Christianity has been a real but catastrophic-cost exit — pursued by millions and punished by loss of community and continuing discrimination. Reservation policy and anti-atrocity law have shifted their position; segregated hamlets, manual scavenging, and caste violence persist.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_bahujan_communities, payer,
    powerless, biographical, constrained, continental).

% Women across castes bear the corpus's gender provisions layered on caste position: the pativrata ideal, sanction for child marriage, widow austerity, restricted inheritance, and seclusion norms calibrated by caste rank — with upper-caste women's purity policing enforcing the hierarchy through their bodies. Compliance is enforced by family and community rather than courts, and their sense of self forms inside the roles, making exit psychologically as well as materially costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, caste_oppressed_women, payer,
    powerless, biographical, identity_locked, continental).

% Phuleite, Ambedkarite, and Periyarite movements and their contemporary heirs. Barred for centuries from the interpretive conversation — forbidden Vedic study, excluded from the councils where the rules were made — they now contest the framework from outside its channels: temple-entry satyagraha, mass conversion, printing presses, political parties, and scholarship. They bear the enforcement backlash and seek the framework's wholesale replacement.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, anti_caste_movements, excluded,
    organized, generational, mobile, continental).

% The Republic of India's courts and legislature. Article 17 abolishes untouchability; the Prevention of Atrocities Act criminalizes caste violence; courts adjudicate when personal-law claims drawn from the corpus collide with fundamental rights. It observes, records, and partially supersedes the arrangement without administering it.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, constitutional_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahmin_ritual_elites).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes ritual procedure, marriage and inheritance rules, dispute-resolution categories, and occupational interdependence across a vast, plural population; provides a shared normative vocabulary and a training pipeline (guru-shishya transmission) that reproduces legal-religious expertise.
% TRANSFER_FUNCTION: Moves ritual fees (dakshina), hereditary labor, deference, educational access, and marriage-alliance value upward from Shudra, Dalit, and women's seats to Brahmin and dominant-caste seats; concentrates interpretive authority in a hereditary male elite.
% ABSENT_VOICES: For most of the corpus's history the people governed by its harshest provisions — Shudras, Dalits, and all women — were barred from Vedic study and from the assemblies where interpretation happened; the texts' unanimity reflects who was allowed in the room. Their objections enter the record only from the nineteenth century onward, through print, movement politics, and constitutional litigation.
% DISAPPEARANCE_RATIONALE: Temple economies, priestly livelihoods, personal-law advocacy, caste-status claims, and endogamous kinship networks all cite the corpus as warrant. Overnight removal would strip the legitimation layer from these arrangements immediately — courts, monastic orders, and households would lose their common reference — while jati's material base (occupational clustering, social capital, matrimonial markets) would decay far more slowly. That gap between instant legitimation-collapse and slow material decay is precisely the abolitionist complaint: the structure outlives its warrant.
% FOUNDING_PROBLEM: Kings and communities needed authoritative rulings on conduct, ritual validity, inheritance, penance, and punishment across a plural subcontinent; the texts consolidated such rulings and tied royal legitimacy to upholding the varnashrama order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: B.R. Ambedkar's Annihilation of Caste (1936) argues the corpus's operative content is hierarchy maintenance; critical philology (e.g., redaction-critical dating of the smritis to specific elite male authorship circles) attests the texts' human, contingent composition rather than timeless revelation; Article 17 of the Indian Constitution records the state's judgment that the arrangement's core prescription is illegitimate. Only the generic need for normative order survives, and it is now met by democratic-constitutional means.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type is snare because this reading holds the coordination story (normative order) to be cover over an extraction core whose persistence has always required enforcement — purity discipline, social boycott, violence against transgressors — and whose exits (conversion, migration, literacy) were systematically suppressed. The metrics describe the arrangement's actual operation at interval end: extractiveness 0.72 reflects severe but legally contested extraction (manual scavenging, caste atrocities, matrimonial endogamy persist after formal abolition); suppression 0.46 reflects enforcement capacity that the constitutional state has dismantled at the formal level while informal enforcement persists; theater_ratio 0.58 marks the point where legitimation performance (citation, recitation, symbolic authority) outweighs functioning governance — courts do not apply the smritis, and personal-law invocation is selective. Suppression is authored as a raw structural property and is not scaled by power or scope; the engine scales only extractiveness, by directionality and spatial scope. All three tracked series run on one shared seven-point grid (1800–2026) so no metric row borrows another's end-state values. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: pre-colonial kingdoms and colonial administrations enforced varna duties, while the republic criminalizes that enforcement — a falling trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute very differently. From the brahmin_ritual_elites seat the same corpus presents as sacred duty and learned vocation — a coordination achievement it staffs and benefits from; from the dalit_bahujan seat it presents as unredeemed extraction enforced by violence; from a reformist interpreter's seat it splits into salvageable core and disposable shell. The orthodox seat would likely compute a low-extraction coordination type; this story's abolitionist seat computes a snare. The engine derives each seat's classification from the structural data; the divergence between seats is the measurement, and the divergence between this reading's claim and any sibling's claim is carried by the linked sibling stories, not reconciled here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (brahmin_ritual_elites, landholding_dominant_castes, upper_caste_households) place those seats near the beneficiary end of d — the arrangement subsidizes them — and identity lock (lineage vocation, endogamous kinship) binds them further in. Victim declarations (dalit_bahujan_communities, shudra_service_castes, caste_oppressed_women) place those seats near the target end; constrained exits (conversion at catastrophic cost, village-level sanction) deny them arbitrage-grade relief, pushing effective extraction toward the full-target ceiling. upper_caste_households are dual-positioned — gaining status while paying fees — and sit mid-range. anti_caste_movements collect nothing and bear enforcement backlash; the constitutional_state observes without collecting. Continental spatial scope amplifies verification difficulty and hence effective extraction on the target side. On the receipt surface: the corpus's distinctive rents (interpretive monopoly, dakshina flows) accrue demonstrably to the interpreter class, so gain_flow names brahmin_ritual_elites; landed-caste gains ride on the broader hierarchy rather than the textual framework specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview drives the analysis: the founding problem (the royal-communal need for authoritative dharmic rulings tying legitimacy to the varnashrama order) is dead — its generic residue, the need for normative order, is met by constitutional-democratic means — while the arrangement persists and the world would still rearrange around its loss. Dead founding problem plus live dependence is the capture/zombie signature, which is exactly this reading's thesis: a framework maintained by inertia, identity, and interest after its warrant lapsed. The rising theater_ratio series operationalizes the same judgment temporally: legitimation performance has overtaken function. Classifying as snare rather than rope prevents the coordination cover story from laundering the extraction; the decayed suppression series prevents overstating present coercive capacity — the arrangement now extracts more through identity and structure than through enforceable command, which is why the abolitionist remedy targets the framework itself rather than merely its enforcement. fixing_cost is prohibitive: for the seats able to change the arrangement, wholesale abandonment costs more than the harms they themselves bear — the classic asymmetry that keeps a captured structure in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (abolitionist_rejection) of the dharmasastra_corpus kernel; is the abolitionist identification of the corpus''s operative essence correct, or does it mistake a historically contingent accretion for the whole?',
    'Cross-reading comparison of the sibling stories'' epsilon authorships over the same referent, combined with philological mapping of where varna provisions sit in the corpus''s textual layers.',
    'If hierarchy is contingent accretion, the reformist reading''s lower epsilon is the better measurement and abolition is over-broad; if constitutive, this reading''s epsilon stands and the sibling readings under-measure the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame indexicality: this constraint is one reading of a contested kernel, and its classification is reading-indexed.').

omega_variable(
    sibling_structural_delta,
    'What would each sibling reading change structurally if adopted — orthodox_literalist zeroes the victim set (provisions become duties) and restores full legitimacy; reformist_contextual splits the victim set between time-bound prescriptions and a cleansed ethical core — and where exactly is the disagreement located?',
    'Locate the disputed element: whether varna/jati ranking is separable from the corpus''s normative authority. Test whether any redaction of the corpus that preserves its authority while deleting rank survives the tradition''s own internal criteria.',
    'Determines whether the family''s three stories measure one arrangement or three; classification transfers between readings only if the disputed element is genuinely separable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Sibling-delta routing: what the other readings would change and where the disagreement sits.').

omega_variable(
    text_vs_material_base_causality,
    'Does the textual framework cause caste extraction, or does it legitimate a structure with independent material bases (land control, labor monopoly, kinship capital)?',
    'Compare regions and communities with similar jati economics but differing degrees of textual attachment; track whether hermeneutic abandonment without land and labor reform changes material outcomes.',
    'If the texts merely legitimate, the epsilon attributable to this constraint is lower than authored and the effective remedy is material redistribution rather than hermeneutic abolition alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_material_base_causality, empirical, 'Whether extraction runs through the texts or through a material base the texts decorate.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (sanction, violence, economic dependency) or internalized (graded identity fusion — subordinated groups defending the hierarchy, as in sanskritization)?',
    'Post-exit trajectory: track converts and urban migrants across generations; if hierarchy-defending attitudes persist after sanctions cease, the internalized share is substantial.',
    'If internalized, effective suppression is higher than the structural measure suggests — targets carry the hierarchy with them after exit — explaining persistence despite enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism in a caste-order constraint.').

omega_variable(
    naturality_contest,
    'Is the varna order a discovered cosmic or moral fact (the orthodox claim) or a constructed power arrangement (this reading''s claim)?',
    'Historical-genetic evidence: the corpus''s own internal variation — regional nibandhas contradicting one another on rank and practice — is inconsistent with a single discovered order; redaction history shows rank rules accumulating under identifiable elite interests.',
    'If constructed, no natural-law certification is available to the arrangement and its persistence requires ongoing enforcement and identity maintenance; if the orthodox claim held, the classification landscape changes entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_contest, conceptual, 'Naturality ambiguity: discovered order versus constructed hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1800, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1850, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1850, 0.19).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1900, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1900, 0.26).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1950, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1950, 0.36).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1956, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1956, 0.43).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t1990, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1990, 0.51).
narrative_ontology:measurement(dharmasastra_abolitionist_tr_t2026, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1800, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1800, 0.92).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1850, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1850, 0.9).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1900, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1900, 0.87).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1950, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1950, 0.81).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1956, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1956, 0.77).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t1990, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement(dharmasastra_abolitionist_be_t2026, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1800, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1800, 0.86).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1850, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1850, 0.79).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1900, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1950, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1956, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1956, 0.5).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t1990, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1990, 0.47).
narrative_ontology:measurement(dharmasastra_abolitionist_su_t2026, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2026, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, enforcement_mechanism).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Dharmasastra' decomposes into three structurally distinct constraints sharing one kernel. This abolitionist reading authors epsilon 0.72 for the standing arrangement (irredeemable extraction, victims named, authority rejected wholesale). The orthodox_literalist sibling authors low epsilon over the same texts (eternal duty, minimal extraction, beneficiaries coordinate). The reformist_contextual sibling authors intermediate epsilon (time-bound caste accretions extractive; separable ethical core exempt). The upstream sibling (orthodox) supplies the legitimacy claims this reading attacks; the reformist sibling mediates. Each member links the others via affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
