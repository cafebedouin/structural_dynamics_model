% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Property Right (Corporate Enclosure Reading)
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   Under the corporate enclosure reading of the Copyright Clause, copyright
 *   is framed as a property right requiring maximal protection against
 *   erosion over time. 'Limited times' is reinterpreted as unlimited
 *   legislative discretion to extend terms—maximal extension short of
 *   explicit perpetuity. This reading benefits incumbent copyright holders
 *   (Disney, RIAA, MPAA, major publishers) who lobby for term extensions
 *   (Sonny Bono Act 1998) and broad anticircumvention provisions (DMCA 1998).
 *   It extracts from derivative creators, educators, archivists, and the
 *   public domain. The constitutional text is the fixed kernel; this reading
 *   instantiates it by interpreting 'limited' to mean 'Congress decides the
 *   limit.' Sibling readings interpret 'limited times' as an enforceable
 *   constitutional floor (public_scaffold_reading) or as a zone of deference
 *   under rational basis review without maximality requirement
 *   (judicial_ambiguity_reading).
 *
 * KEY AGENTS:
 *   - Incumbent copyright holders (Disney, RIAA, MPAA, major publishers) — agenda-setters, institutional power, collect extended monopoly rents, drive legislative capture
 *   - Derivative creators (remixers, visual artists, novelists adapting existing work) — victims, moderate power, constrained by licensing costs and legal barriers
 *   - Educators and educational platforms — victims, organized power, bear rising licensing costs, restricted fair use for curricula
 *   - Archivists and preservation organizations — victims, moderate power, criminally exposed for preservation efforts that circumvent DRM
 *   - Public domain researchers — victims, powerless, trapped without right-holder cooperation or licensing budget
 *   - Congress — agenda-setter, institutional power, captured by incumbent beneficiaries through lobbying, interprets 'limited times' as discretionary
 *   - Courts — observer, institutional power, defer via rational basis review (Eldred), lock in enclosure reading judicially
 *   - Contemporary creators — contradictory beneficiary/payer, benefit from copyright of own work, pay extraction cost of extended incumbency
 *   - Public — excluded, organized power, bear diffuse costs, absent from policy conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.71).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Right (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '54056cd6-b3ec-4919-8c7d-aed151c982f6').
narrative_ontology:cs_kernel_codification('54056cd6-b3ec-4919-8c7d-aed151c982f6', fixed_text).
narrative_ontology:cs_authority_grounding('54056cd6-b3ec-4919-8c7d-aed151c982f6', extraction).
narrative_ontology:cs_interpretation_layer_present('54056cd6-b3ec-4919-8c7d-aed151c982f6').
narrative_ontology:cs_reading_relation('54056cd6-b3ec-4919-8c7d-aed151c982f6', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('54056cd6-b3ec-4919-8c7d-aed151c982f6', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('54056cd6-b3ec-4919-8c7d-aed151c982f6', foundational, property_right_maximalism).
narrative_ontology:cs_axiom_status(property_right_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('54056cd6-b3ec-4919-8c7d-aed151c982f6', property_right_maximalism, deontological).
narrative_ontology:cs_axiom('54056cd6-b3ec-4919-8c7d-aed151c982f6', foundational, limited_times_as_legislative_discretion).
narrative_ontology:cs_axiom_status(limited_times_as_legislative_discretion, holdable).
narrative_ontology:cs_axiom_grounding('54056cd6-b3ec-4919-8c7d-aed151c982f6', limited_times_as_legislative_discretion, conventional).
narrative_ontology:cs_reference_frame('54056cd6-b3ec-4919-8c7d-aed151c982f6', property_right_maximalism).
narrative_ontology:cs_drift_state('54056cd6-b3ec-4919-8c7d-aed151c982f6', contemporary_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('54056cd6-b3ec-4919-8c7d-aed151c982f6', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, contemporary_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, contemporary_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major entertainment corporations (Disney, RIAA, MPAA members, major publishing houses) set the legislative agenda for copyright extension through lobbying, campaign finance, and captured regulatory bodies. They frame copyright as inviolable property requiring maximum protection against time decay. They benefit directly from every term extension—works remain under monopoly control longer, generating rental income without ongoing creation. They actively defend against fair use expansion, circumvention restrictions, and public domain enrichment.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, musicians, filmmakers, and remixers who want to build on existing copyrighted work face legal barriers, licensing costs, and cease-and-desist actions. The extended copyright term means that seminal 20th-century works they wish to adapt remain locked in corporate ownership. They must license at rates set by incumbents or operate in legal shadow. Their creative practice is constrained by the copyright monopoly's scope and term.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Universities, schools, and educational platforms pay licensing fees to use copyrighted materials (textbooks, images, film clips, music) in curricula. The extended copyright term and restricted fair-use interpretation increase these costs over time. They navigate conflicting compliance signals: teach critical skills using contemporary media, but that media remains locked. Fair use doctrine provides some cover, but litigation risk deters aggressive fair-use interpretation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Libraries, archives, and preservation organizations face legal barriers to digitizing, preserving, and providing access to copyrighted materials even after creators are dead and works are culturally historical. Extended copyright terms mean materials that would have entered the public domain decades ago remain legally inaccessible. The DMCA's anticircumvention provisions criminalize preservation efforts that break digital locks, even on abandoned works. Identity locked to preservation mission: cannot exit without abandoning archival practice.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, identity_locked, national).

% Scholars, linguists, and computational researchers studying cultural patterns depend on access to large corpora of text, music, and images. Extended copyright terms and anticircumvention rules prevent legal text mining and computational analysis of copyrighted material. Their research is trapped between copyright law (prohibits copying without licensing) and practical impossibility (no rights-holder exists, or licensing costs billions). They have no mechanism to participate in the copyright policy conversation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers, payer,
    powerless, generational, trapped, global).

% Enacts copyright legislation. Structurally captured by incumbent beneficiaries through lobbying and campaign finance. Interprets 'limited times' clause as delegating term-setting to legislative discretion, not as an enforceable limit. Has repeatedly extended copyright term (Sonny Bono Act 1998, Digital Millennium Copyright Act 1998) without narrowing scope or conditioning extension on public benefit.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress, agenda_setter,
    institutional, biographical, arbitrage, national).

% Interpret the Copyright Clause and statutory law. They have largely deferred to Congress on term length via rational-basis review (Eldred v. Ashcroft 2003), treating 'limited times' as non-judicially-enforceable. They narrowly construe fair use and aggressively apply anticircumvention provisions. Their interpretive posture locks the corporate enclosure reading into place.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, courts, observer,
    institutional, generational, analytical, national).

% Current-generation authors, musicians, and filmmakers benefit from copyright protection of their own work but increasingly discover that extended terms lock in earlier generations' work, constraining their own adaptive and derivative practice. The coordination benefit (copying protection during commercial life) is real; the extraction cost (term extension beyond commercial viability, fair use restriction) has grown. They occupy a contradictory seat.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, contemporary_creators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, contemporary_creators, payer).

% Citizens and culture consumers who would benefit from a richer public domain, lower educational costs, and freer access to cultural works are structurally excluded from copyright policy debates. They bear diffuse costs (higher textbook prices, reduced access to archived materials, constrained adaptive media) but have no organized voice in legislative or judicial proceedings. They would argue for the public-scaffold reading if seated.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Copyright provides incentive for creation and first-publication by granting temporary monopoly: creators have motive to invest in production and take publication risk; the public receives works they would not otherwise exist. Copyright law offers predictability: investors know the legal scope of protected rights.
% TRANSFER_FUNCTION: Transfers economic rent from public consumers (licensing fees, higher educational costs, restricted derivative use) and from derivative creators (who must license to adapt) to incumbent copyright holders. The transfer mechanism is the copyright monopoly's scope (what uses are restricted) and term (how long monopoly lasts). Extended term and restricted fair use amplify the transfer without expanding coordination benefit.
% ABSENT_VOICES: Public-domain advocates, open-culture movements, derivative creators, educators bearing licensing costs, and archivists restricted from preservation are systematically absent from legislative and judicial copyright proceedings. They would argue copyright should be shorter and narrower. Their absence from the policy conversation is maintained by the same institutional structure (capture of Congress, deferential judicial review) that sustains the corporate enclosure reading.
% DISAPPEARANCE_RATIONALE: If copyright law reverted to the Framers' text ('limited times' as enforceable shorter terms, broad fair use, no anticircumvention provisions), publishing markets would reorganize around shorter-lived exclusive periods and front-loaded compensation; derivative creativity would expand immediately; educational costs would drop; archivists and researchers would gain access to 20th-century materials; public domain would expand dramatically. The copyright-dependent rental stream would collapse. Incumbent incumbents would face unrecovered losses on rights purchased under the long-term regime.
% FOUNDING_PROBLEM: Framers observed that absent copyright protection, authors had no incentive to publish widely, investors in printing had no protection against immediate copying, and public received fewer books and improved works. They granted temporary monopoly as a deal: creators get exclusive period, public gets the work afterwards and can build on it.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent beneficiaries (RIAA, MPAA) attest the founding problem (inadequate creation incentive) remains live and requires maximal protection. Independent economists and copyright scholars (Lessig, Benkler, Fisher) attest the founding problem is solved: creation incentive is very high for works with commercial potential; most works earn bulk revenue in first 10–20 years; copyright extension captures no additional incentive (authors created before knowing of extension). Legislative testimony from educators, archivists, and public-domain advocates (outside the beneficiary set) supports the 'problem solved, extraction persists' reading. International comparative data (EU 70-year, Canada 50-year terms) shows no correlation between term length and creative output.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58 → 0.82 over 1976–2024) because copyright term extensions systematically transfer value from public/future creators to incumbent holders without expanding creation incentive—the founding problem (inadequate incentive) is solved decades before term expires. The measure rose sharply at 1998 (Sonny Bono/DMCA) when term was extended 20 years and anticircumvention criminalized, making the extraction both durable and enforceable. Theater ratio rises (0.22 → 0.48) because the justification story (incentivizing creation) decouples from function (maintaining monopoly rentals on works created under prior law). By 2024, term extensions no longer affect incentive for creation of contemporary works (incentive is established within first 2–5 years of commercial release); theater reflects defensive lobbying and litigation to maintain legacy monopolies. Suppression is high and growing because the regime requires active enforcement: cease-and-desist letters, litigation against circumvention and fair use, DMCA prosecutions, DRM technical locks. Courts enforce through deferential review; Congress through captured legislation; firms through aggressive copyright enforcement. Accessibility collapse is moderate-high (0.64) because alternatives exist (public domain works, fair use where broad, works from non-incumbents) but are shrinking as term extends and fair use narrows. Resistance is substantial (0.58) from academic, archival, and open-culture movements, but fails at legislative/judicial level because incumbent voice is concentrated and funded.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (incumbent beneficiaries + Congress captured by them), copyright is legitimate property protection and 'limited times' is Congress's to set—maximal protection is justified. From the payer seats (derivative creators, educators, archivists), the same structure is extractive monopoly maintenance using constitutional property language as cover. Courts occupy a technical observer position but defer to Congress via rational basis review, which locks the enclosure reading into place judicially. The engine will compute divergent types across seats because the structural data (beneficiary/victim, suppression, exit options) differs radically by position.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders: d near 1.0 (full target inverted—they are the beneficiaries; d = 0.0). They set the agenda, face no exit, collect rents indefinitely. Congress: d near 0.0 (beneficiary of campaign contributions and lobbying from incumbents; pays no cost). Courts: d near 0.5 (symmetric observer; enforce neither benefits nor harms them directly). Derivative creators: d near 1.0 (trapped victims; pay licensing costs, face legal barriers, constrained exit). Educators: d near 0.9 (victims; organized exit possible but costly and risky; licensing costs are unavoidable). Archivists: d near 0.95 (victims; criminally exposed; identity-locked to preservation mission; exit only by abandoning archival practice). Public: d near 0.95 (trapped by diffuse cost, organized excluded). Contemporary creators: d near 0.3 (contradictory; benefit from copyright of own work, but pay extraction cost of extended incumbency and restricted fair use for their own adaptive practice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authors need incentive to create and publish) is live in the corporate enclosure reading's own framing. But empirically, creation incentive is established within 2–5 years of commercial release for works that will be commercially viable at all. Copyright extension beyond 70+ years (current US term for corporate works: 95 years after publication or 120 years after creation) creates zero marginal incentive. The constraint persists not because it solves the founding problem, but because it collects rents on works created under prior legal regimes. This is the textbook mandatrophy signature: the problem it was built for is solved decades before the constraint expires, but the constraint persists theatrically. The classification as tangled_rope (not snare) rests on the genuine coordination function (incentive + publication infrastructure) being real, even though that function is exhausted long before term ends. The tangled_rope gate requires beneficiaries (inc. holders) + victims (derivative creators, etc.) + active enforcement (DMCA, litigation, DRM) — all are present. The extraction is not pure (snare) because the coordination function is real at the margin; it is tangled because the coordination function is exhausted while extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_decay,
    'At what point in a work''s commercial life has the creation incentive been fully realized, making further copyright term extension non-marginal to incentive?',
    'Empirical economic analysis of publishing, music, and film revenue streams: do works earn bulk revenue in first decade, or does revenue remain distributed across 50+ year periods? Comparative international data from jurisdictions with shorter terms (EU 70-year, Canada 50-year) vs. US 95-year—do shorter-term jurisdictions produce fewer works or lower-quality works?',
    'If creation incentive is realized within 10–20 years and further extension produces zero marginal incentive, the founding problem is dead, mandatrophy is confirmed, and the classification shifts from tangled_rope (mixed coordination/extraction) to snare (pure extraction with cover story). If term length materially affects incentive across the full 95-year period, the coordination function is live and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_decay, empirical, 'Whether copyright term extension beyond 20 years affects creation incentive').

omega_variable(
    legislative_capture_mechanism,
    'Is the corporate enclosure reading sustained by genuine democratic preference for maximal copyright, or by regulatory capture of Congress by incumbent beneficiaries?',
    'Polling of constituent preferences: do voters support 95-year copyright terms or do they support shorter terms when asked directly? Campaign finance analysis of copyright legislation: do bills with corporate sponsor support pass at higher rates? Comparative legislative history: did copyright term length increase in jurisdictions with lower corporate lobbying influence, or only in captured jurisdictions?',
    'If sustained by genuine preference, the reading is democratically legitimate even if extractive; if sustained by capture, the reading is procedurally illegitimate and vulnerable to constitutional challenge via regulatory capture doctrine. This affects the narrative authority of Congress as an agenda-setter (are they captured or reflecting constituency?) and the justification for the extracted arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_capture_mechanism, empirical, 'Whether the enclosure reading is sustained by capture or democratic preference').

omega_variable(
    fair_use_internal_contradiction,
    'Can copyright simultaneously be maximal property (the enclosure reading''s foundational axiom) and maintain a meaningful fair use carve-out that permits unrestricted adaptive, critical, and educational use?',
    'Jurisprudential analysis: do courts enforce fair use broadly (contradicting maximal property principle) or narrow it to near-zero (confirming maximal property)? Empirical measurement: what proportion of copyright cases filed by incumbents succeed against fair-use defenses?',
    'If fair use is meaningfully enforced, copyright is not maximal property — it is constrained property with mandatory exceptions. The foundation axiom shifts from property_right_maximalism to property_right_with_exceptions. If fair use is narrowed to near-zero (current trend in caselaw), maximal property is confirmed, but that confirms the extraction, and eliminates the coordination function benefit to derivative creators and educators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_internal_contradiction, conceptual, 'Whether maximal property can coexist with meaningful fair use').

omega_variable(
    reading_alternative_framing_asymmetry,
    'Is the corporate enclosure reading the only coherent instantiation of ''limited times'' in the Copyright Clause, or are the sibling readings (public_scaffold, judicial_ambiguity) equally defensible from the constitutional text?',
    'Constitutional law scholarship and litigation outcomes: Do courts treat only the maximal reading as the defensible interpretation of ''limited times,'' or do they acknowledge multiple live readings and choose among them? Historical framing: did the Framers intend a property-maximalist reading, a public-domain-protective reading, or a delegated-discretion reading?',
    'If multiple readings are equally textually defensible, this omega documents the reading under-determination: this story describes one coherent reading, but the kernel permits others. If the maximal reading is the only defensible one, this omega resolves toward the maximalist side—but that means the other stories in the constraint family describe incoherent misreadings, not equal instantiations. This affects how the committer structure routes through the constraint network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_framing_asymmetry, conceptual, 'Whether maximal property reading is the only coherent instantiation of ''limited times''').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(copy_tr_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1988, 0.28).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.38).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(copy_tr_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement(copy_be_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1988, 0.65).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.76).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(copy_be_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.54).
narrative_ontology:measurement(copy_su_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1988, 0.59).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.66).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(copy_su_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.25).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anticircumvention_provision).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_term_extension_trajectory).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Copyright Clause kernel. Sibling readings (public_scaffold_reading, judicial_ambiguity_reading) instantiate alternative interpretations of 'limited times' as an enforceable floor vs. as delegated legislative discretion. The three readings have different beneficiaries, different ε values, and different classifications. They are linked by network.affects_constraints because shifts in one reading's institutional authority affect the others: if courts adopted the public_scaffold reading and enforced a shorter constitutional term, the corporate_enclosure_reading's legal ground would collapse. The constraint family documents the same kernel (the Copyright Clause) under three distinct readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
